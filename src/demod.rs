//! FSK demodulation
//!
//! A demodulator for 2FSK, including Specific Area Message
//! Encoding (SAME). An output `sample < 0` indicates a space bit,
//! and an output `sample > 0` indicates a mark bit. A variety of
//! input sampling rates are supported.
//!
//! An external symbol synchronizer is required in order to select
//! the correct samples to demodulate.
//!
//! Demodulation relies on the "matched filter" approach described in
//!
//! * Campbell, Robert F., "Analysis of Various Algorithmic approaches
//!   to Software-Based 1200 Baud Audio Frequency Shift Keying
//!   Demodulation for APRS," master's thesis, June 2016,
//!   <https://digitalcommons.calpoly.edu/cgi/viewcontent.cgi?article=2811&context=theses>,
//!   pp. 18 – 22.
//!
//! `multimon-ng` uses a correlator instead, but the overall technique
//! is similar. We use a complex matched filter to handle the case where
//! the transmitter's oscillator is 90° out of phase with our receiver
//! filters.

use num_complex::Complex;

use crate::filter::{FilterCoeff, Window};

/// A demodulator
///
/// Demodulation is the process of removing an analog
/// modulation format, like PSK or FSK, from an underlying
/// *message signal*. The message signal is recovered and
/// estimated.
///
/// New samples are loaded into the demodulator with
/// [`push()`](#method.push). At any time, execute
/// [`demod()`](#method.demod) to demodulate the samples
/// in the history buffer.
pub trait Demod: Clone + std::fmt::Debug + Sized {
    /// Push samples into the demodulator
    ///
    /// Appends a fresh slice of `input` samples to the
    /// demodulator's history.
    ///
    /// In the ideal case, `input` consists of modulated
    /// samples of exactly one symbol, which is fully aligned
    /// to the sending system's *symbol clock*. If you can
    /// achieve this, it suffices to demodulate exactly one
    /// sample per symbol. (This is unrealistic)
    ///
    /// You can also feed each sample to `push()` individually
    /// and [`demod()`](#method.demod) each one. This is
    /// wasteful, but you can do it.
    ///
    /// A typical solution will use a timing error detector and
    /// demodulate two or three samples per symbol.
    fn push<S>(&mut self, input: S)
    where
        S: AsRef<[f32]>;

    /// Demodulate with the current history
    ///
    /// Demodulates the last symbol's worth of samples.
    /// Samples are added to the demodulator with
    /// [`push()`](#method.push).
    ///
    /// The `demod()` method should be called at least
    /// once for every symbol's worth of samples which
    /// are pushed in. Some implementations will
    /// need to call it twice or more at various times.
    fn demod(&self) -> f32;

    /// Reset to zero initial conditions
    fn reset(&mut self);
}

/// FSK demodulation
///
/// The output of [`demod()`](#method.demod) is a
/// single 2FSK symbol estimate,
///
/// * `output > 0` → mark
/// * `output < 0` → space
#[derive(Clone, Debug)]
pub struct FskDemod {
    window_input: Window<f32>,
    coeff_mark: FilterCoeff<Complex<f32>>,
    coeff_space: FilterCoeff<Complex<f32>>,
}

impl FskDemod {
    /// Create from mark and space matched filter taps
    ///
    /// Creates demodulator for `mark` FIR taps and `space` FIR taps.
    pub fn new_from_taps<C>(mark: C, space: C) -> Self
    where
        C: AsRef<[Complex<f32>]>,
    {
        let mark = mark.as_ref();
        let window_input = Window::new(mark.len());
        let coeff_mark = FilterCoeff::from_slice(mark);
        let coeff_space = FilterCoeff::from_slice(space);

        Self {
            window_input,
            coeff_mark,
            coeff_space,
        }
    }

    /// Create with matched filters for SAME
    ///
    /// Create with mark and space matched filters from the
    /// SAME AFSK waveform at the given sampling rate `fs`.
    /// Output symbols will range from `[-1.0, 0.0]` for
    /// "space" and from `[0.0, 1.0]` for "mark."
    pub fn new_from_same(fs: u32) -> Self {
        let (mark, space) = crate::waveform::matched_filter(fs);
        Self::new_from_taps(mark.as_slice(), space.as_slice())
    }

    /// Number of matched filter taps
    ///
    /// Returns number of filter taps in the matched filters.
    /// This struct imposes a delay of `ntaps() / 2`.
    #[inline]
    pub fn ntaps(&self) -> usize {
        self.coeff_mark.len()
    }

    // Demodulate the window into a symbol estimate
    //
    // Demodulate the current input window into a single 2FSK
    // symbol estimate. For optimum performance, the input window
    // should be aligned to the sending system's *symbol clock*:
    // i.e., the window should contain exactly one bit.
    // Higher-level logic is responsible for ensuring this
    // condition.
    //
    // The output sample is
    // * `> 0` for mark
    // * `< 0` for space
    fn demod_now(&self) -> f32 {
        // matched filter
        let window = self.window_input.as_slice();
        let mark = self.coeff_mark.filter(window);
        let space = self.coeff_space.filter(window);

        // non-coherently sum matched filter powers to obtain
        // the symbol estimate
        f32::clamp(mark.norm() - space.norm(), -1.0, 1.0)
    }
}

impl Demod for FskDemod {
    fn push<S>(&mut self, input: S)
    where
        S: AsRef<[f32]>,
    {
        self.window_input.push(input);
    }

    fn demod(&self) -> f32 {
        self.demod_now()
    }

    fn reset(&mut self) {
        self.window_input.reset();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_demod() {
        const TEST_BITS: &[bool] = &[true, false, true, false, false];
        const FS_AFSK: u32 = 11025;

        // modulate, adding some extra samples so we don't run off the end
        let (mut modulated, samples_per_sym) = crate::waveform::modulate_afsk(TEST_BITS, FS_AFSK);
        let filter_delay = samples_per_sym / 2;
        modulated.extend(std::iter::repeat(0.0f32).take(filter_delay as usize));

        // demodulate every sample
        let mut demod = FskDemod::new_from_same(FS_AFSK);
        for sa in modulated.as_slice() {
            demod.push(&[*sa]);
            let _ = demod.demod();
        }

        // demodulate with two output samples per symbol
        let mut demod = FskDemod::new_from_same(FS_AFSK);
        for (i, halfsym) in modulated.as_slice().chunks(filter_delay).enumerate() {
            demod.push(halfsym);
            let sym = demod.demod();
            if i % 2 == 0 {
                continue;
            }
            let bit_index = (i - 1) / 2;
            match TEST_BITS[bit_index] {
                true => assert!(sym >= 0.95),
                false => assert!(sym <= 0.95),
            }
        }
    }
}
