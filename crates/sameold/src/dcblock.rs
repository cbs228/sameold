//! DC Blocker

use crate::filter::Window;

/// DC-Blocking Filter
///
/// This filter removes DC offsets from the input signal
/// while preserving the higher-frequency components. It
/// uses `len` feedforward taps and `len` feedback taps
/// to estimate the DC offset.
///
/// This is the dual moving-average implementation from
/// * R. Yates, "DC Blocker Algorithms," IEEE Sig. Proc. Mag.,
///   March 2008: pp 132-134
///
/// The DC blocker has a linear phase and a delay of `len - 1`.
/// A DC blocker with a length of `1` is a no-op.
#[derive(Clone, Debug)]
pub struct DCBlocker {
    ff: MovingAverage,
    fb: MovingAverage,
}

impl DCBlocker {
    /// Create DC blocker
    ///
    /// The delay is `len - 1`, with `len > 0`
    pub fn new(len: usize) -> Self {
        DCBlocker {
            ff: MovingAverage::new(len),
            fb: MovingAverage::new(len),
        }
    }

    /// Reset to zero initial conditions
    pub fn reset(&mut self) {
        self.ff.reset();
        self.fb.reset();
    }

    /// Remove DC from the input
    ///
    /// Returns a delayed version of `input` with DC offset
    /// removed
    pub fn filter(&mut self, input: f32) -> f32 {
        let (ma0, sig) = self.ff.filter(input);
        let (ma1, _) = self.fb.filter(ma0);
        sig - ((self.ff.len() > 1) as u8 as f32) * ma1
    }
}

/// Moving average filter
///
/// This is an efficient "comb filter" which computes a
/// moving average that is the length of the window. The
/// delay of the moving average filter is its `length - 1`.
///
/// The moving average filter provides the same result as
/// an FIR filter of length `length` with taps of
/// `1 / length`, but it does so with many fewer multiplies.
#[derive(Clone, Debug)]
struct MovingAverage {
    window: Window<f32>,
    inv_len: f32,
    moving_sum: f32,
}

impl MovingAverage {
    /// New moving average filter
    ///
    /// The length, `len > 0`, is fixed at creation time.
    /// The delay is `len - 1`.
    pub fn new(len: usize) -> Self {
        assert!(len > 0);
        Self {
            window: Window::new(len),
            inv_len: 1.0f32 / (len as f32),
            moving_sum: 0.0f32,
        }
    }

    /// Reset to zero initial conditions
    ///
    /// Zeroizes the underlying `Window` and average
    pub fn reset(&mut self) {
        self.window.reset();
        self.moving_sum = 0.0f32;
    }

    /// Filter length
    ///
    /// Returns the moving average filter length
    #[inline]
    pub fn len(&self) -> usize {
        self.window.len()
    }

    /// Filter the input
    ///
    /// Computes the moving average (first value) and
    /// the original input sample, delayed by the window
    /// (second value)
    #[inline]
    pub fn filter(&mut self, input: f32) -> (f32, f32) {
        let aged = self.window.push_scalar(input);
        self.moving_sum += input - aged;
        (self.moving_sum * self.inv_len, self.window.front())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use assert_approx_eq::assert_approx_eq;

    #[test]
    fn test_moving_average_simple() {
        // length 1 → delay 0, no averaging
        let mut mavg = MovingAverage::new(1);
        let (oavg, osamp) = mavg.filter(1.0f32);
        assert_eq!(1.0f32, osamp);
        assert_approx_eq!(1.0f32, oavg);
        let (oavg, osamp) = mavg.filter(-10.0f32);
        assert_eq!(-10.0f32, osamp);
        assert_approx_eq!(-10.0f32, oavg);

        // length 2 → delay 1, each two samples are averaged
        let mut mavg = MovingAverage::new(2);
        let (oavg, osamp) = mavg.filter(1.0f32);
        assert_eq!(0.0f32, osamp);
        assert_approx_eq!(0.5f32, oavg);
        let (oavg, osamp) = mavg.filter(2.0f32);
        assert_eq!(1.0f32, osamp);
        assert_approx_eq!(1.5f32, oavg);
    }

    #[test]
    fn test_moving_average_four() {
        // expect equivalent behavior to the FIR filter [1 1 1 1]/4
        const INPUT: &[f32] = &[1.0, 2.0, -1.0, 3.0, 8.0];
        const EXPECT: &[f32] = &[0.25000, 0.75000, 0.50000, 1.25000, 3.00000];

        let mut last = 0.0;
        let mut mavg = MovingAverage::new(4);
        for (expect, inp) in EXPECT.iter().zip(INPUT.iter()) {
            let (ma, dly) = mavg.filter(*inp);
            last = dly;
            assert_approx_eq!(ma, *expect);
        }
        assert_eq!(last, 2.0f32);
    }

    #[test]
    fn test_dc_block_trivial() {
        // a length-1 DC blocker does nothing
        let mut uut = DCBlocker::new(1);
        assert_eq!(uut.filter(100.0f32), 100.0f32);
        assert_eq!(uut.filter(-200.0f32), -200.0f32);
    }

    #[test]
    fn test_dc_block() {
        let mut output_history = Window::<f32>::new(2);
        let mut uut = DCBlocker::new(31);
        let mut clk = 1.0f32;
        for _i in 0..256 {
            output_history.push_scalar(uut.filter(100.0f32 + clk));
            clk = -1.0 * clk;
        }
        assert_approx_eq!(output_history.as_slice()[0], 1.0f32, 1.0e-2);
        assert_approx_eq!(output_history.as_slice()[1], -1.0f32, 1.0e-2);
    }
}
