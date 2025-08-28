//! Decision feedback equalizer and slicer
//!
//! The decision feedback equalizer (DFE) performs the
//! following operations:
//!
//! 1. Channel impulse response estimation
//! 2. Hard decision slicing
//! 3. Bits to bytes
//!
//! The [`Equalizer`] operates on
//! two samples per symbol and collects estimated symbols
//! into bytes. Bytes are transmitted least-significant
//! bit (LSb) first. The "hard decision" slicer logic is
//! intended for a binary constellation with +1 and -1
//! symbols.
//!
//! FM tends to attenuate high frequencies more than low
//! frequencies. Some receivers will try to correct for
//! this with pre-emphasis/de-emphasis filters… others,
//! not so much. The adaptive equalizer relieves us of the
//! burden of accounting for this.
//!
//! MathWorks has a reasonable introduction to adaptive equalizers:
//! <https://www.mathworks.com/help/comm/ug/adaptive-equalizers.html>.
//! Our implementation is based on `run_me_bpsk.m` from
//! <https://github.com/vineel49/dfe/>, with NLMS instead of LMS.
//! Further reading on NLMS can be found
//! <https://matousc89.github.io/padasip/sources/filters/nlms.html>.

use super::filter::{FilterCoeff, Window};

#[cfg(not(test))]
use log::debug;

#[cfg(test)]
use std::println as debug;

/// No training sequence defined; can't enter training mode
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NoTrainingSequenceErr;

/// An adaptive decision feedback equalizer (DFE)
///
/// The decision feedback equalizer (DFE) reduces
/// inter-sample interference (ISI) by estimating
/// the impulse response of the channel.
///
/// It is assumed that the input to the `Equalizer`
/// is already *byte-aligned*. The input must be
/// at two samples per symbol and provided in
/// slices of 16 samples. The samples which contain
/// the symbol estimate must be at odd indices, like
///
/// ```no_run
/// let syms = [0.0f32, 1.0, 0.0, -1.0];
/// let bits = [true, false];
/// ```
///
/// At the output, symbols are hard-sliced into bits
/// (for the last time) and grouped into bytes. The
/// Equalizer requires that the bytes be transmitted
/// least-significant bit (LSb) first.
///
/// The Equalizer can have training mode, which
/// matches known sync patterns. When the equalizer
/// is commanded into training mode with
/// [`train`](#method.train), the subsequent
/// decisions are ignored. The training bytes are
/// used instead until they are exhausted.
#[derive(Clone, Debug)]
pub struct Equalizer {
    // the distance to move each new impulse response
    // estimate to the zero posteriori error point:
    // 1.0 goes all the way, while 0.0 doesn't move at
    // all. Sometimes called "mu" or "gain"
    relaxation: f32,

    // a main-diagonal weighting constant that helps keep
    // matrices invertible. Sometimes called "delta." Set
    // to zero to disable regularization
    regularization: f32,

    // training bits (LSb transmitted first)
    train_to: Option<u32>,

    // feed-forward coefficients
    feedforward_coeff: FilterCoeff<f32>,

    // feedback coefficients
    feedback_coeff: FilterCoeff<f32>,

    // feed-forward filter window
    feedforward_wind: Window<f32>,

    // feedback filter window
    feedback_wind: Window<f32>,

    // controls off/on/training mode
    mode: EqualizerState,
}

#[allow(dead_code)]
impl Equalizer {
    /// Input length, in samples
    pub const INPUT_LENGTH: usize = 16;

    /// Create equalizer
    ///
    /// The equalizer will have `nfeedforward` feed-forward
    /// coefficients and `nfeedback` feedback coefficients.
    ///
    /// `relaxation`: the distance to move each new impulse
    /// response estimate to the zero posteriori error point:
    /// 1.0 goes all the way, while 0.0 doesn't move at all.
    /// Sometimes called "mu" or "gain"
    ///
    /// `regularization`: a main-diagonal weighting constant
    /// that helps keep matrices invertible. Sometimes called
    /// "delta." Set to zero to disable regularization.
    ///
    /// `train_to` is a byte pattern (as a `u32`) which will
    /// be used for training. The transmitter must transmit
    /// least-significant bit (LSb) first.
    pub fn new(
        nfeedforward: usize,
        nfeedback: usize,
        relaxation: f32,
        regularization: f32,
        train_to: Option<u32>,
    ) -> Self {
        let feedforward_coeff = FilterCoeff::from_identity(nfeedforward);
        let feedback_coeff = FilterCoeff::from_identity(nfeedback);
        let feedforward_wind = Window::new(nfeedforward);
        let feedback_wind = Window::new(nfeedback);

        assert_eq!(feedforward_coeff.inner().len(), feedforward_wind.len());
        assert_eq!(feedback_coeff.inner().len(), feedback_wind.len());

        Self {
            relaxation,
            regularization,
            train_to,
            feedforward_coeff,
            feedback_coeff,
            feedforward_wind,
            feedback_wind,
            mode: EqualizerState::EnabledFeedback,
        }
    }

    /// Estimate one byte of output
    ///
    /// Process a slice of 16 samples (8 symbols)(8 bits), which
    /// must already be aligned to a byte boundary by
    /// [`CodeAndPowerSquelch`](super::codesquelch::CodeAndPowerSquelch)
    /// or some other mechanism. The `byte_samples` must have
    /// symbol estimates in odd indices like
    ///
    /// ```txt
    /// let byte_samples = [0.0f32, 1.0, 0.0, -1.0, …];
    /// ```
    ///
    /// The estimated byte and the adaptive filter error, as of
    /// the last bit, is output.
    ///
    /// If you know that the training sequence has just started,
    /// you should call [`train()`](#method.train) first to
    /// enter training mode. In training mode, the training bytes
    /// will be output regardless of the input.
    ///
    /// This method may panic if `byte_samples` is not exactly
    /// one byte in length (and will panic in debug mode).
    pub fn input(&mut self, byte_samples: &[f32]) -> (u8, f32) {
        assert_eq!(byte_samples.len(), Self::INPUT_LENGTH);

        let mut byte = 0;
        let mut last_err = 0.0f32;

        for (bitind, twosamp) in byte_samples.chunks(2).enumerate() {
            let (bit, err) = self.estimate_symbol(twosamp);
            last_err = err;
            byte |= (bit as u8) << bitind;
        }

        (byte, last_err)
    }

    /// Reset to zero initial conditions
    ///
    /// The enable/disable state is preserved.
    pub fn reset(&mut self) {
        self.feedforward_coeff.identity();
        self.feedback_coeff.identity();
        self.feedforward_wind.reset();
        self.feedback_wind.reset();
    }

    /// Enable or disable the adaptive algorithm
    ///
    /// Disabling the adaptive algorithm will freeze the filter
    /// coefficients. You may also want to `reset()` the filter
    /// coefficients to their default / "no-op" initial values.
    pub fn enable(&mut self, enable: bool) {
        self.mode = match enable {
            true => EqualizerState::EnabledFeedback,
            false => EqualizerState::Disabled,
        }
    }

    /// Puts adaptive equalizer into training mode
    ///
    /// The adaptive equalizer is enabled and reset to the start
    /// of the training sequence. The next 32 symbols (64 samples)
    /// input *must* be the training sequence, properly synchronized
    /// in time.
    pub fn train(&mut self) -> Result<(), NoTrainingSequenceErr> {
        let train_to = self.train_to.ok_or(NoTrainingSequenceErr)?;
        self.mode = EqualizerState::EnabledTraining(train_to, 0);
        Ok(())
    }

    /// True if the adaptive behavior is enabled
    ///
    /// When enabled, the adaptive filter is allowed to evolve.
    /// When disabled, filtering is still performed, but the
    /// adaptive filter is not allowed to evolve.
    pub fn is_enabled(&self) -> bool {
        self.mode != EqualizerState::Disabled
    }

    /// True if the adaptive equalizer is in training mode
    pub fn is_training(&self) -> bool {
        if let EqualizerState::EnabledTraining(_, _) = self.mode {
            true
        } else {
            false
        }
    }

    // Evaluates filters against the input
    //
    // The input must be a "zero" sample and a symbol sample,
    // in that order.
    //
    // Returns a tuple of `(decision, error)`, where
    // `decision` is a hard-sliced bit estimate and `error`
    // is the difference between the filter output and the
    // estimated symbol.
    fn estimate_symbol(&mut self, input: &[f32]) -> (bool, f32) {
        assert_eq!(2, input.len());

        // run feedforward and feedback filter estimates to get a symbol estimate
        self.feedforward_wind.push(input);
        let ff = self.feedforward_coeff.filter(&self.feedforward_wind);
        let fb = self.feedback_coeff.filter(&self.feedback_wind);

        // best soft estimate of the symbol possible
        let sym_val = ff - fb;

        // estimate symbol and error
        let out = match self.mode {
            EqualizerState::Disabled => {
                // hard decision slicer, no error
                (sym_val.signum(), 0.0f32)
            }
            EqualizerState::EnabledFeedback => {
                // hard decision slicer
                let sym_est = sym_val.signum();

                // assuming our decision is correct,
                // what is the error vs the estimate?
                let err = sym_est - sym_val;

                // run adaptive algorithm
                self.evolve(err);

                (sym_est, err)
            }
            EqualizerState::EnabledTraining(mut sa, mut count) => {
                // compute symbol estimate from training sequence
                let sym_est = (2.0f32 * (sa & 0x1u32) as f32) - 1.0f32;
                sa = sa >> 1;

                // what is the error vs the estimate?
                let err = sym_est - sym_val;

                // run adaptive algorithm
                self.evolve(err);

                count += 1;
                if count >= 32 {
                    // we're done
                    debug!("equalizer: end training with err: {:.4}", err);
                    self.mode = EqualizerState::EnabledFeedback;
                } else {
                    self.mode = EqualizerState::EnabledTraining(sa, count)
                }

                (sym_est, err)
            }
        };

        // add symbol estimate to the feedback filter for later
        self.feedback_wind.push(&[out.0, 0.0f32]);

        // return bit estimate and error
        (out.0 >= 0.0f32, out.1)
    }

    // Evolve filter taps
    //
    // Perform a NLMS update of both the feedforward and the
    // feedback taps.
    #[inline]
    fn evolve(&mut self, error: f32) {
        nlms_update(
            self.relaxation,
            self.regularization,
            error,
            &self.feedforward_wind,
            self.feedforward_coeff.as_mut(),
        );

        assert_eq!(self.feedback_wind.inner().len(), self.feedback_coeff.len());
        nlms_update(
            self.relaxation,
            self.regularization,
            -error,
            &self.feedback_wind,
            self.feedback_coeff.as_mut(),
        );
    }
}

// Equalizer state
#[derive(Clone, Debug, PartialEq, Eq)]
enum EqualizerState {
    // Not evolving
    Disabled,

    // Evolving, with live decision-feedback
    EnabledFeedback,

    // Evolving, with the given training sample and counter
    EnabledTraining(u32, u32),
}

// Normalized Least Mean Squares (NLMS) update
//
// NLMS is a variant of Least Mean Squares which uses a
// variable gain. The gain is computed based on the power
// of the input. The given `filter` taps are updated based
// on the input samples in `window`.
fn nlms_update<W>(relaxation: f32, regularization: f32, error: f32, window: W, filter: &mut [f32])
where
    W: IntoIterator<Item = f32>,
    W::IntoIter: DoubleEndedIterator + Clone,
{
    let window = window.into_iter();
    let gain = nlms_gain(relaxation, regularization, window.clone());
    for (coeff, data) in filter.iter_mut().zip(window) {
        *coeff += gain * error * data;
    }
}

// Compute NLMS gain
//
// ```txt
// gain =               relaxation
//        --------------------------------------
//          regularization + norm(window, 2)^2
// ```
//
// where `norm(x, 2)` is the L² norm of `x`
#[inline]
fn nlms_gain<'a, W>(relaxation: f32, regularization: f32, window: W) -> f32
where
    W: IntoIterator<Item = f32>,
{
    let mut sumsq = 0.0f32;
    for w in window {
        sumsq += w * w;
    }

    relaxation / (regularization + sumsq)
}

impl std::fmt::Display for NoTrainingSequenceErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no training sequence defined")
    }
}

impl std::error::Error for NoTrainingSequenceErr {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::super::waveform;
    use super::*;

    use assert_approx_eq::assert_approx_eq;

    /// The Proakis B fading channel
    const PROAKIS_B: &[f32] = &[0.407, 0.815, 0.407];

    // simple feedback system operation: impulse response is identity
    #[test]
    fn test_estimate_symbol_simple() {
        const INPUT: &[f32] = &[0.0f32, 0.5f32, 0.0f32, -0.5f32];

        let mut eqlz = Equalizer::new(8, 4, 0.2, 1.0e-5, None);
        eqlz.enable(false);

        let out: Vec<(bool, f32)> = INPUT
            .chunks(2)
            .map(|samps| eqlz.estimate_symbol(samps))
            .collect();

        assert_eq!(2, out.len());
        assert_eq!(true, out[0].0);
        assert_approx_eq!(out[0].1, 0.0f32);
        assert_eq!(false, out[1].0);
        assert_approx_eq!(out[1].1, 0.0f32);

        // now try with adaptive filtering
        eqlz.enable(true);

        let out: Vec<(bool, f32)> = INPUT
            .chunks(2)
            .cycle()
            .take(32)
            .map(|sample| eqlz.estimate_symbol(sample))
            .collect();

        // the adaptive filter drives the error to zero
        assert!(out[out.len() - 1].1.abs() < 1.0e-5f32);
    }

    // Test NLMS
    //
    // Feed a test sequence through a Proakis B channel. Assume that,
    // like an acoustic echo canceler, we can compute the error
    // signal. Can we evolve a filter to match the channel?
    #[test]
    fn test_nlms_evolve() {
        const INPUT: &[f32] = &[0.0f32, 1.0f32, 0.0f32, -1.0f32];
        const RELAXATION: f32 = 0.10f32;
        const REGULARIZATION: f32 = 1.0e-6f32;

        // this is what we're about to do the poor signal
        let channel_coeff = FilterCoeff::from_slice(PROAKIS_B);
        let mut channel_wind = Window::<f32>::new(PROAKIS_B.len());

        // and this is the inverse filter we evolve to match it
        let mut inverse_coeff = FilterCoeff::<f32>::from_identity(3);
        let mut inverse_wind = Window::<f32>::new(3);

        let mut err = 0.0f32;

        for sample in INPUT.iter().cycle().take(128) {
            // corrupt with channel
            channel_wind.push(&[*sample]);
            let ch_sample = channel_coeff.filter(&channel_wind);

            // filter with the inverse filter we're evolving
            inverse_wind.push(&[ch_sample]);
            let est_sample = inverse_coeff.filter(&inverse_wind);

            // error estimate
            err = sample - est_sample;

            // NLMS update
            nlms_update(
                RELAXATION,
                REGULARIZATION,
                err,
                &inverse_wind,
                inverse_coeff.as_mut(),
            );
        }

        assert!(err.abs() < 1e-2);
    }

    // Test a complicated channel
    #[test]
    fn test_estimate_symbol_proakis() {
        const CHANNEL_COEFF: &[f32] = &[0.8f32, -0.2f32]; // tiny bit of ISI
        const INPUT: &[f32] = &[0.0f32, 1.0f32, 0.0f32, -1.0f32];

        // this is what we're about to do the poor signal
        let channel_coeff = FilterCoeff::from_slice(CHANNEL_COEFF);
        let mut channel_wind = Window::<f32>::new(CHANNEL_COEFF.len());

        let mut uut = Equalizer::new(8, 4, 0.2, 1.0e-5, None);

        // train on 32 repetitions of input
        let mut last = (false, 0.0f32);
        for samples in INPUT.chunks(2).cycle().take(32) {
            let mut ch_samples = [0.0f32, 0.0f32];
            for (inp, outp) in samples.iter().zip(ch_samples.iter_mut()) {
                channel_wind.push(&[*inp]);
                *outp = channel_coeff.filter(&channel_wind);
            }

            last = uut.estimate_symbol(&ch_samples);
        }
        assert!(last.1.abs() < 1e-4);

        // now make sure we get it right
        for samples in INPUT.chunks(2) {
            let mut ch_samples = [0.0f32, 0.0f32];
            for (inp, outp) in samples.iter().zip(ch_samples.iter_mut()) {
                channel_wind.push(&[*inp]);
                *outp = channel_coeff.filter(&channel_wind);
            }

            last = uut.estimate_symbol(&ch_samples);
            assert_eq!(samples[1] >= 0.0f32, last.0);
            assert!(last.1.abs() < 1e-4);
        }
    }

    // Test a complicated channel
    #[test]
    fn test_estimate_symbol_training() {
        let mut uut = Equalizer::new(8, 4, 0.2, 1.0e-5, Some(waveform::PREAMBLE_SYNC_WORD));
        uut.train().expect("training mode");
        assert_eq!(
            uut.mode,
            EqualizerState::EnabledTraining(waveform::PREAMBLE_SYNC_WORD, 0)
        );

        // let's see what happens when we train on a set of symbols
        // but receive the exact opposite of it
        let chansig = waveform::bytes_to_samples(&[0x54, 0x54], 2);

        let _rxsig0: Vec<(bool, f32)> = chansig
            .chunks(2)
            .map(|sa| uut.estimate_symbol(sa))
            .collect();

        // halfway through training
        match uut.mode {
            EqualizerState::EnabledTraining(_, 16) => assert!(true),
            _ => unreachable!(),
        }

        let _rxsig1: Vec<(bool, f32)> = chansig
            .chunks(2)
            .map(|sa| uut.estimate_symbol(sa))
            .collect();

        // we enter feedback mode when we're done.
        assert_eq!(uut.mode, EqualizerState::EnabledFeedback);

        // now transmit the opposite of that. What do we get?
        let rx = uut.estimate_symbol(&[0.0f32, -1.0f32]);

        // What‽ we transmitted a false, but we got a true
        assert!(rx.0);
        // We've managed to evolve a DFE that flips our bits!
        // Get that training sequence right… or else!

        // train it right this time
        uut.reset();
        uut.train().expect("training mode err");

        let chansig = waveform::bytes_to_samples(&[0xAB, 0xAB, 0xAB, 0xAB], 2);
        let _rxsig3: Vec<(bool, f32)> = chansig
            .chunks(2)
            .map(|sa| uut.estimate_symbol(sa))
            .collect();
        assert_eq!(uut.mode, EqualizerState::EnabledFeedback);

        let rx = uut.estimate_symbol(&[0.0f32, -1.0f32]);
        assert!(!rx.0);
    }

    #[test]
    fn test_input() {
        let chansig = waveform::bytes_to_samples(&[0xAB, 0xBA], 2);
        let mut uut = Equalizer::new(8, 4, 0.2, 1.0e-5, None);
        let out: Vec<(u8, f32)> = chansig.chunks(16).map(|sa| uut.input(sa)).collect();

        assert_eq!(out.len(), 2);
        assert_eq!(out[0].0, 0xAB);
        assert_eq!(out[1].0, 0xBA);
    }
}
