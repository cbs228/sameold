//! Automatic gain control

/// Automatic gain control
///
/// The `Agc` attempts to normalize the output signal
/// level to an average output absolute value of 1.0.
/// This makes a full-scale sinusoid range from
/// -1.0 to +1.0.
///
/// While floating-point numbers are *relatively*
/// insensitive to loss of arithmetic precision,
/// symbol timing recovery works best when the output
/// has a known signal level. The `Agc` helps make
/// this happen.
///
/// The `Agc` uses a simple feedback filter with a
/// single tap. It can be locked to prevent its
/// gain value from changing.
#[derive(Clone, Debug)]
pub struct Agc {
    // AGC update bandwidth: higher→faster
    bandwidth: f32,

    // cap minimum gain
    min_gain: f32,

    // cap maximum gain
    max_gain: f32,

    // if true, prevent power estimate from changing
    locked: bool,

    // computed gain
    gain: f32,
}

impl Agc {
    /// New AGC
    ///
    /// Creates automatic gain control with the given loop
    /// `bandwidth`, which is specified as a fraction of the
    /// sampling rate. A bandwidth of `0.0` prevents the filter
    /// from updating at all, while a bandwidth of `1.0` will
    /// change the gain completely each sample. (You don't
    /// want this.)
    ///
    /// The maximum gain will be clamped to between `min_gain`
    /// and `max_gain`, in units of input. (i.e., a "voltage.")
    pub fn new(bandwidth: f32, min_gain: f32, max_gain: f32) -> Self {
        Self {
            bandwidth: f32::clamp(bandwidth, 0.0f32, 1.0f32),
            min_gain,
            max_gain,
            locked: false,
            gain: f32::min(1.0f32, min_gain),
        }
    }

    /// Reset to zero initial conditions
    pub fn reset(&mut self) {
        self.gain = 1.0f32;
        self.locked = false;
    }

    /// Normalize input sample
    ///
    /// Applies and updates filter gain. Assuming the
    /// `input` signal is zero-mean, the output is the
    /// `input` normalized to values between `(-1.0, +1.0)`.
    /// If the AGC is unlocked, the gain value is updated.
    #[inline]
    pub fn input(&mut self, input: f32) -> f32 {
        let out = input * self.gain;
        self.gain += (!self.locked as u8 as f32) * (1.0f32 - out.abs()) * self.bandwidth;
        self.gain = f32::clamp(self.gain, self.min_gain, self.max_gain);
        out
    }

    /// Lock the gain
    ///
    /// If `lock` is true, further updates to the AGC's gain
    /// value are prevented until the AGC is unlocked or
    /// [`reset()`](#method.reset).
    pub fn lock(&mut self, lock: bool) {
        self.locked = lock;
    }

    /// Obtain gain value
    ///
    /// Obtain the current gain value used to normalize the
    /// input. This value is multiplied by the input to
    /// produce the gain-controlled signal.
    pub fn gain(&self) -> f32 {
        self.gain
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use assert_approx_eq::assert_approx_eq;

    #[test]
    fn test_agc() {
        let mut agc = Agc::new(0.05, 0.0, 1.0e6);

        let mut val = 0.0f32;
        for _i in 0..256 {
            val = agc.input(-2.0f32);
        }

        assert_approx_eq!(agc.gain, 0.5f32);
        assert_approx_eq!(val, -1.0f32);

        // can we lock?
        agc.reset();
        agc.lock(true);

        for _i in 0..16 {
            val = agc.input(-2.0f32);
        }
        assert_eq!(agc.gain, 1.0f32);
        assert_approx_eq!(val, -2.0f32);
    }
}
