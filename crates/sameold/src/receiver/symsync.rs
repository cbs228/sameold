//! Symbol timing clock
//!
//! A timing loop for symbol timing recovery. The Timing Error
//! Detector (TED) uses the zero-crossing method. The timing loop
//! is based on GNU Radio's
//! [symbol_sync_ff](https://www.gnuradio.org/doc/doxygen/classgr_1_1digital_1_1symbol__sync__ff.html)
//! block, which uses a proportional integrate (PI) filter to
//! estimate the average and current sample clock periods.
//!
//! From the available stream of samples, the `TimingLoop`
//! chooses two samples for every symbol. Samples are selected
//! such that the "half sample" between any two symbol
//! transitions is approximately *zero*. Finding the zeros also
//! finds the point where the matched filter's magnitude is
//! maximized.
//!
//! ```txt
//! Matched filter output x[n]
//!  /\
//!   |     1Ts
//! M |.....x
//! A |      .
//! R |       .
//! K |        .
//!   |         .
//! --|----------x----------> time (Ts)
//!   |           .       .
//! S |            .     .
//! P |             .   .
//! A |              . .
//! C |               x
//! E |               2Ts
//! ```
//!
//! Unlike the gnuradio implementation, the
//! [TimingLoop](struct.TimingLoop.html) is designed to work
//! with high oversampling factors at the input. Oversampling
//! the input makes interpolation unnecessary.
//!
//! The `TimingLoop` uses a floating-point sample clock to
//! request samples for the timing error detector. Clock
//! mismatch due to integer sampling rates is accounted for
//! and folded in to the next cycle.
//!
//! For best performance, the inputs to the timing loop should
//! be normalized to a signal value in `±1.0`.

use arraydeque::ArrayDeque;

/// Symbol estimate with timing error
#[derive(Clone, Debug, Default, PartialEq)]
pub struct SymbolEstimate {
    /// Data samples
    ///
    /// `data[0]` is a "zero"
    /// `data[1]` is a soft symbol estimate
    pub data: [f32; 2],

    /// Timing error estimate
    ///
    /// Error is expressed as a fraction of the symbol rate
    ///
    /// `err < 0`  → timing late
    /// `err == 0` → timing prompt
    /// `err > 0`  → timing early
    ///
    /// An `err` value of ±1.0 indicates that the sample clock
    /// is exactly one-half sample off, which is the worst-case
    /// error.
    pub err: f32,
}

#[allow(dead_code)]
impl SymbolEstimate {
    /// New symbol estimate
    ///
    /// Accepts zero estimate, symbol estimate, and error estimate
    pub fn new(zero: f32, sym: f32, err: f32) -> Self {
        Self {
            data: [zero, sym],
            err,
        }
    }

    /// Symbol estimate
    pub fn sym(&self) -> f32 {
        self.data[1]
    }
}

/// Symbol clock tracking loop
///
/// The clock tracking loop is a Proportional Integrate (PI)
/// filter which tracks both the average and instantaneous
/// clock period.
///
/// Unlike the gnuradio implementation, this loop does not
/// support interpolated sampling. For SAME/EAS, we have
/// *plenty* of input samples without having to interpolate.
#[derive(Clone, Debug)]
pub struct TimingLoop {
    // average input samples per timing error detector input
    samples_per_ted: f32,

    // minimum permitted period_avg (slowest permitted sample clock)
    period_min: f32,

    // maximum permitted period_avg (fastest permitted sample clock)
    period_max: f32,

    // proportional gain of the PI filter
    //
    // gain for updating the instantaneous period
    loop_alpha: f32,

    // integral gain of the PI filter
    //
    // gain for updating the average period
    loop_beta: f32,

    // average number of input samples per TED input
    period_avg: f32,

    // instantaneous number of input samples per TED input
    period_inst: f32,

    // timing error detector
    ted: ZeroCrossingTed,
}

impl TimingLoop {
    /// New timing loop
    ///
    /// Creates a new timing loop which expects `samples_per_symbol`
    /// *input* samples per symbol, on average, with a maximum clock
    /// deviation of `max_deviation` symbol periods.
    ///
    /// The tracking loop smooths error estimates with a proportional
    /// integrate (PI) filter. The bandwidth of this filter is
    /// `loop_bandwidth`, which is expressed as a fraction of the
    /// timing error detector's input rate.
    pub fn new(samples_per_symbol: f32, loop_bandwidth: f32, max_deviation: f32) -> Self {
        const NEED_SPS: f32 = ZeroCrossingTed::SAMPLES_PER_SYMBOL as f32;

        let (loop_alpha, loop_beta) = compute_loop_alphabeta(loop_bandwidth);
        let samples_per_ted = samples_per_symbol / NEED_SPS;
        let period_deviation = samples_per_symbol * f32::clamp(max_deviation, 0.0, 0.5);
        let period_avg = samples_per_ted;
        let period_inst = samples_per_ted;
        let period_min = period_avg - period_deviation;
        let period_max = period_avg + period_deviation;

        Self {
            samples_per_ted,
            period_min,
            period_max,
            loop_alpha,
            loop_beta,
            period_avg,
            period_inst,
            ted: ZeroCrossingTed::default(),
        }
    }

    /// Reset to zero initial conditions
    pub fn reset(&mut self) {
        self.ted.reset();
        self.period_avg = self.samples_per_ted;
        self.period_inst = self.samples_per_ted;
    }

    /// Set the loop bandwidth
    ///
    /// Sets the control loop bandwidth to `loop_bandwidth`, which
    /// should be a fraction of the output symbol rate.
    pub fn set_loop_bandwidth(&mut self, loop_bandwidth: f32) {
        let (loop_alpha, loop_beta) = compute_loop_alphabeta(loop_bandwidth);
        self.loop_alpha = loop_alpha;
        self.loop_beta = loop_beta;
    }

    /// Process input sample
    ///
    /// Accepts a demodulated `sample` and the `offset` between
    /// the last commanded sample time and the actual commanded
    /// sample time, `-0.5 < offset < +0.5`. The offset accounts
    /// for the fact that we can only sample at discrete
    /// times—i.e., the input sample rate. The offset is positive
    /// if `sample` is before the requested time and negative if
    /// the sample is after the requested time.
    ///
    /// Returns a tuple of
    ///
    /// 1. The number of input samples until the next call to
    ///    `input()`
    ///
    /// 2. A symbol estimate, if a new symbol is ready
    pub fn input(&mut self, sample: f32, offset: f32) -> (f32, Option<SymbolEstimate>) {
        let sym = self.ted.input(sample);
        (self.advance_loop(offset, &sym), sym)
    }

    /// Average number of samples between inputs
    ///
    /// Returns the *average* period between successive calls
    /// to [`input()`](#method.input). This is a fractional
    /// number of samples at the input rate.
    pub fn samples_per_ted(&self) -> f32 {
        self.samples_per_ted
    }

    // Advance timing loop
    //
    // `offset` is the difference between the last commanded
    // sample time and the actual sample time. `sym` is the
    // symbol and timing estimate. Returns the number of
    // input samples until the next timing error detector
    // update.
    fn advance_loop(&mut self, offset: f32, sym: &Option<SymbolEstimate>) -> f32 {
        let offset = offset.clamp(-0.5f32, 0.5f32);

        match sym {
            Some(sym) => {
                // correct error estimate for reported sample time
                let err = f32::clamp(sym.err - offset / self.samples_per_ted, -1.0f32, 1.0f32);

                // integral arm
                self.period_avg += self.loop_beta * err;
                self.period_avg = self.period_avg.clamp(self.period_min, self.period_max);

                // proportional arm
                // we can't go back in time; we must go forward instead
                self.period_inst = self.period_avg + self.loop_alpha * err + offset;
                if self.period_inst < 0.0f32 {
                    self.period_inst = self.period_avg;
                }
            }
            None => {
                self.period_inst += offset;
            }
        }

        self.period_inst
    }
}

/// Zero-crossing timing error detector
#[derive(Clone, Debug)]
pub struct ZeroCrossingTed {
    history: ArrayDeque<f32, 3, arraydeque::Wrapping>,
    sample_counter: u32,
}

#[allow(dead_code)]
impl ZeroCrossingTed {
    /// Number of input samples per symbol that this TED requires
    pub const SAMPLES_PER_SYMBOL: u32 = 2;

    /// Create timing detector
    pub fn new() -> Self {
        Self::default()
    }

    /// Reset to zero initial conditions
    pub fn reset(&mut self) {
        self.history.clear();
        for _i in 0..self.history.capacity() {
            self.history.push_back(0.0f32);
        }
        self.sample_counter = 0;
    }

    /// Estimate symbol timing error
    ///
    /// Accepts `sample` inputs at approximately half the
    /// symbol rate. Produces one timing estimate for every
    /// three samples provided.
    pub fn input(&mut self, sample: f32) -> Option<SymbolEstimate> {
        self.history.push_back(sample);
        self.sample_counter = (self.sample_counter + 1) % ZeroCrossingTed::SAMPLES_PER_SYMBOL;
        if self.sample_counter == 1 {
            let err = zero_crossing_metric(&self.history);
            Some(SymbolEstimate::new(self.history[1], self.history[2], err))
        } else {
            None
        }
    }
}

impl Default for ZeroCrossingTed {
    fn default() -> Self {
        let mut out = ZeroCrossingTed {
            history: ArrayDeque::default(),
            sample_counter: 0,
        };
        out.reset();
        out
    }
}

// Compute the Zero Crossing metric for a signal sampled at twice the symbol rate
//
// v[0] is previous symbol
// v[1] is inter-sample zero
// v[2] is most-recent symbol
//
// output is the timing error estimate
//
// this is a modified Gardener metric which uses hard decision slicing
#[inline]
fn zero_crossing_metric<A>(v: &A) -> f32
where
    A: std::ops::Index<usize, Output = f32> + ?Sized,
{
    v[1] * (fsk_decision(v[0]) - fsk_decision(v[2]))
}

// Simple hard decision slicer for 2FSK
#[inline]
fn fsk_decision(sym: f32) -> f32 {
    sym.signum()
}

// Compute PI alpha and beta
//
// Computes loop `(alpha, beta)` given the `loop_bandwidth` as
// a fraction of the symbol rate. Assumes a critically-damped
// system.
fn compute_loop_alphabeta(loop_bandwidth: f32) -> (f32, f32) {
    let omega_n_norm = 2.0f32 * std::f32::consts::PI * loop_bandwidth;
    let k0 = 2.0f32;
    let k1 = f32::exp(-omega_n_norm);
    let sinh_zeta_omega_n_t = f32::sinh(omega_n_norm);
    let alpha = k0 * k1 * sinh_zeta_omega_n_t;
    let beta = k0 * (1.0f32 - k1 * (sinh_zeta_omega_n_t + 1.0f32));
    (alpha, beta)
}

#[cfg(test)]
mod tests {
    use super::*;

    use assert_approx_eq::assert_approx_eq;
    use nalgebra::DVector;

    // generate single period of a sinusoid
    fn gen_sinusoid(period: usize) -> DVector<f32> {
        let twopi = 2.0f32 * std::f32::consts::PI;
        DVector::from_iterator(
            period,
            (0..period)
                .into_iter()
                .map(|n| f32::sin(twopi * (n as f32) / (period as f32))),
        )
    }

    #[test]
    fn test_zero_crossing_metric() {
        const DEAD_ON: &[f32] = &[1.0, 0.0, -1.0];
        const DEAD_ON_LOW_LEVEL: &[f32] = &[-1.0, 0.0, 1.0];
        const CONSTANT_HIGH: &[f32] = &[1.0, 1.0, 1.0];
        const CONSTANT_LOW: &[f32] = &[-1.0, -1.0, -1.0];
        const TIMING_EARLY: &[f32] = &[0.8, 0.2, -0.8];
        const TIMING_LATE: &[f32] = &[0.8, -0.2, -0.8];

        assert_approx_eq!(zero_crossing_metric(DEAD_ON), 0.0f32);
        assert_approx_eq!(zero_crossing_metric(DEAD_ON_LOW_LEVEL), 0.0f32);
        assert_approx_eq!(zero_crossing_metric(CONSTANT_HIGH), 0.0f32);
        assert_approx_eq!(zero_crossing_metric(CONSTANT_LOW), 0.0f32);
        assert_approx_eq!(zero_crossing_metric(TIMING_EARLY), 0.4f32);
        assert_approx_eq!(zero_crossing_metric(TIMING_LATE), -0.4f32);
    }

    #[test]
    fn test_compute_loop_alphabeta() {
        let (alpha, beta) = compute_loop_alphabeta(0.0f32);
        assert_approx_eq!(alpha, 0.0f32);
        assert_approx_eq!(beta, 0.0f32);

        let (alpha, beta) = compute_loop_alphabeta(0.5f32);
        assert_approx_eq!(alpha, 0.99813f32, 1.0e-4);
        assert_approx_eq!(beta, 0.91544f32, 1.0e-4);

        let (alpha, beta) = compute_loop_alphabeta(1.0f32);
        assert_approx_eq!(alpha, 1.0f32, 1.0e-4);
        assert_approx_eq!(beta, 0.99627f32, 1.0e-4);
    }

    #[test]
    fn test_zero_crossing_ted() {
        let mut ted = ZeroCrossingTed::new();

        assert!(ted.input(0.8f32).is_some());
        assert!(ted.input(0.2f32).is_none());
        match ted.input(-0.8f32) {
            Some(sym) => {
                assert_eq!(-0.8f32, sym.sym());
                assert_approx_eq!(0.4f32, sym.err);
            }
            _ => unreachable!(),
        }

        assert!(ted.input(0.2f32).is_none());
        match ted.input(0.8f32) {
            Some(sym) => {
                assert_eq!(0.8f32, sym.sym());
                assert_approx_eq!(-0.4f32, sym.err);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_timing_loop_advance() {
        let mut timing = TimingLoop::new(32.0f32, 0.25, 0.125f32);
        assert_approx_eq!(timing.period_inst, 16.0f32);
        assert_approx_eq!(timing.period_max, 16.0f32 + 4.0f32);
        assert_approx_eq!(timing.advance_loop(0.0f32, &None), 16.0f32);
        assert_approx_eq!(timing.advance_loop(0.5f32, &None), 16.5f32);
        assert_approx_eq!(timing.advance_loop(-0.5f32, &None), 16.0f32);
        assert_approx_eq!(timing.advance_loop(-0.5f32, &None), 15.5f32);

        timing.reset();
        assert_approx_eq!(timing.period_inst, 16.0f32);

        assert_approx_eq!(
            timing.advance_loop(0.0f32, &Some(SymbolEstimate::new(0.0f32, 1.0f32, 0.0f32))),
            16.0f32
        );

        // we sampled half a sample early, and our error
        // estimate reflects this; it cancels out
        let early = 0.5f32;
        assert_approx_eq!(
            timing.advance_loop(
                early,
                &Some(SymbolEstimate::new(0.0f32, 0.95f32, early / 16.0f32))
            ),
            16.5f32
        );

        // likewise for sampling late
        assert_approx_eq!(
            timing.advance_loop(
                -early,
                &Some(SymbolEstimate::new(0.0f32, 0.95f32, -early / 16.0f32))
            ),
            15.5f32
        );
    }

    // perform timing test with circular inp vector, return last estimate
    fn timing_test(
        timing: &mut TimingLoop,
        inp: &DVector<f32>,
        start_sample: usize,
        verbose: bool,
    ) -> SymbolEstimate {
        let mut offset = 0.0f32;
        let mut sa = start_sample;
        let mut last_sym = SymbolEstimate::default();
        timing.reset();
        for _i in 0..128 {
            let (skip, sym) = timing.input(inp[sa], offset);

            // skip ahead
            let whole = skip.round();
            offset = skip - whole;

            if verbose {
                println!("skip: {}, whole: {}, offset: {}", skip, whole, offset);
            }

            // wrap to one period
            sa += whole as usize;
            sa = sa % inp.len();

            if let Some(s) = sym {
                last_sym = s;
                if verbose {
                    println!("{:?}", last_sym);
                }
            }
        }
        last_sym
    }

    #[test]
    fn test_timing_loop_bestcase() {
        const SAMPLES_PER_SYMBOL: usize = 32;

        // generate one period of a sinusoid
        //   * symbol value +1 at inp[16]
        //   * symbol value -1 at inp[48]
        //   * inp is periodic
        //
        // Here we use a sinusoid to mimic the output of the
        // matched filter. In reality, the matched filter output
        // does not look particularly sinusoidal, but this is a
        // convenient periodic function we can use.
        let inp = gen_sinusoid(2 * SAMPLES_PER_SYMBOL);
        assert_eq!(2 * SAMPLES_PER_SYMBOL, inp.len());
        assert_approx_eq!(0.0f32, inp[0]);
        assert_approx_eq!(1.0f32, inp[16]);
        assert_approx_eq!(-1.0f32, inp[48]);

        let mut timing = TimingLoop::new(SAMPLES_PER_SYMBOL as f32, 0.25, 0.125f32);

        // test best-case timing error: we are synchronized to start with
        // note: SAME preamble is 16 × 8 bits long
        let last_sym = timing_test(&mut timing, &inp, 16, false);
        assert!(last_sym.sym().abs() > 0.99);
        assert!(last_sym.err < 1e-4);
    }

    #[test]
    fn test_timing_loop_nearworst() {
        const SAMPLES_PER_SYMBOL: usize = 32;

        let inp = gen_sinusoid(2 * SAMPLES_PER_SYMBOL);

        let mut timing = TimingLoop::new(SAMPLES_PER_SYMBOL as f32, 0.25, 0.125f32);

        // test *near* worse-case timing error: we are off by almost
        // a half-sample to start with
        let last_sym = timing_test(&mut timing, &inp, 15, false);
        assert!(last_sym.sym().abs() > 0.99);
        assert!(last_sym.err < 1e-4);
    }

    #[test]
    fn test_timing_loop_worstcase() {
        const SAMPLES_PER_SYMBOL: usize = 32;

        let inp = gen_sinusoid(2 * SAMPLES_PER_SYMBOL);

        let mut timing = TimingLoop::new(SAMPLES_PER_SYMBOL as f32, 0.25, 0.125f32);

        // test worst-case timing error, where we are off by
        // half a sample
        let last_sym = timing_test(&mut timing, &inp, 0, false);
        assert!(last_sym.sym().abs() > 0.99);
        assert!(last_sym.err < 1e-4);
    }

    #[test]
    fn test_timing_loop_misc() {
        const SAMPLES_PER_SYMBOL: usize = 32;

        let inp = gen_sinusoid(2 * SAMPLES_PER_SYMBOL);

        // turn down the bandwidth. do we still make it?
        let mut timing = TimingLoop::new(SAMPLES_PER_SYMBOL as f32, 0.20, 0.125f32);
        let last_sym = timing_test(&mut timing, &inp, 16, false);
        assert!(last_sym.sym().abs() > 0.99);
        assert!(last_sym.err < 1e-4);

        // if we're already nearly synchronized, will a lower bandwidth suffice?
        let mut timing = TimingLoop::new(SAMPLES_PER_SYMBOL as f32, 0.05, 0.125f32);
        let last_sym = timing_test(&mut timing, &inp, 3, false);
        assert!(last_sym.sym().abs() > 0.99);
        assert!(last_sym.err < 1e-4);
    }
}
