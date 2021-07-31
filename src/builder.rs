use crate::receiver::SameReceiver;

/// Builds a SAME/EAS receiver
///
/// The builder comes with a sensible set of default options.
/// All you really need to provide is the input sampling
/// rate. The [`SameReceiver`](struct.SameReceiver.html) was
/// designed to work well at a sampling rate of 22050 Hz,
/// however, and you may wish to tweak some of these values.
///
/// The API specified by the builder is part of this crate's
/// API. The actual default values are *not*, however, and
/// are subject to revision in any minor release. If you
/// care very strongly about a setting, be sure to configure
/// it here.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct SameReceiverBuilder {
    input_rate: u32,
    agc_bandwidth: f32,
    timing_bandwidth_unlocked: f32,
    timing_bandwidth_locked: f32,
    timing_max_deviation: f32,
    squelch_power_open: f32,
    squelch_power_close: f32,
    squelch_bandwidth: f32,
    preamble_max_errors: u32,
    equalizer: Option<EqualizerBuilder>,
    frame_prefix_max_errors: u32,
    frame_max_invalid_bytes: u32,
}

impl SameReceiverBuilder {
    /// New receiver chain with "sensible" defaults
    ///
    /// The only mandatory parameter is the input sampling
    /// rate, in Hz. To avoid computationally-intensive
    /// resampling in your sound server, you should use one
    /// of the native output rates of your sound card or an
    /// easy division thereof. 22050 Hz is a popular choice.
    /// The `SameReceiver` can be designed for a variety of
    /// different sampling rates.
    pub fn new(input_rate: u32) -> Self {
        Self {
            input_rate,
            agc_bandwidth: 0.05f32,
            timing_bandwidth_unlocked: 0.125f32,
            timing_bandwidth_locked: 0.05f32,
            timing_max_deviation: 0.01,
            squelch_power_open: 0.10,
            squelch_power_close: 0.05,
            squelch_bandwidth: 0.125,
            preamble_max_errors: 2,
            equalizer: Some(EqualizerBuilder::default()),
            frame_prefix_max_errors: 2,
            frame_max_invalid_bytes: 10,
        }
    }

    /// Build a receiver chain
    ///
    /// Once built, the receiver chain is immediately ready to
    /// process samples.
    pub fn build(&self) -> SameReceiver {
        SameReceiver::from(self)
    }

    /// Automatic gain control bandwidth (fraction of baud rate)
    ///
    /// Controls how fast the AGC is permitted to update.
    /// Bandwidth is expressed as a fraction of the SAME
    /// baud rate, which is 520.83 Hz. This value may be
    /// greater than 1.0 if you want the AGC to evolve
    /// significantly faster than one symbol.
    pub fn with_agc_bandwidth(&mut self, bw: f32) -> &mut Self {
        self.agc_bandwidth = f32::clamp(bw, 0.0, 1.0);
        self
    }

    /// Timing loop bandwidth (fraction of baud rate)
    ///
    /// The timing loop bandwidth controls how quickly the
    /// symbol timing estimate is allowed to change. There
    /// are two values:
    ///
    /// 1. The first value is used when the system has not
    ///    yet acquired the SAME preamble / byte sync.
    ///
    /// 2. The second value is used when byte sync is
    ///    acquired. The second value is clamped to the
    ///
    /// The loop bandwidth is specified as a fraction of
    /// the SAME baud rate, which is 520.83 Hz.
    pub fn with_timing_bandwidth(&mut self, unlocked_bw: f32, locked_bw: f32) -> &mut Self {
        self.timing_bandwidth_unlocked = f32::clamp(unlocked_bw, 0.0, 1.0);
        self.timing_bandwidth_locked = f32::clamp(locked_bw, 0.0, self.timing_bandwidth_unlocked);
        self
    }

    /// Maximum timing deviation (fraction of baud rate)
    ///
    /// `max_dev` is the maximum permitted deviation from
    /// the ideal SAME baud rate, which is 520.83 Hz.
    /// `max_dev` should be given in fractions of one baud,
    /// where 0.0 represents no deviation and 0.5 represents
    /// an entire half-symbol of deviation. Keep this value
    /// small!
    pub fn with_timing_max_deviation(&mut self, max_dev: f32) -> &mut Self {
        self.timing_max_deviation = f32::clamp(max_dev, 0.0, 0.5);
        self
    }

    /// Squelch power thresholds (linear power)
    ///
    /// Require a minimum symbol power of `open` in order to
    /// begin decoding a frame. Stop decoding a frame if the
    /// power drops below `close`. These values are linear
    /// powers in units of amplitude^2.
    ///
    /// Prior to demodulation, AGC normalizes the received
    /// signal to an amplitude of 1.0. These power values
    /// should range from `[0.0, 1.0]`. A value of 0.0
    /// disables the power squelch, and a value of 1.0
    /// requires a no-noise perfect demodulation.
    ///
    /// We recommend setting `open` ≥ `close`.
    pub fn with_squelch_power(&mut self, open: f32, close: f32) -> &mut Self {
        self.squelch_power_open = f32::clamp(open, 0.0, 1.0);
        self.squelch_power_close = f32::min(close, open);
        self
    }

    /// Squelch power tracker bandwidth (fraction of baud rate)
    ///
    /// The power squelch is smoothed with a single-pole IIR
    /// filter with bandwidth `bw`. The bandwidth is a fraction
    /// of the baud rate.
    pub fn with_squelch_bandwidth(&mut self, bw: f32) -> &mut Self {
        self.squelch_bandwidth = bw;
        self
    }

    /// Maximum preamble sync bit errors
    ///
    /// The `SameReceiver` detects the start of a frame by
    /// correlating with four bytes (32 bits) of the SAME
    /// preamble. The preamble is a total of 16 bytes long.
    /// When acquiring the preamble, we permit some errors
    /// to occur. (We hope that the adaptive equalizer will
    /// lower the error rate farther down the chain.)
    ///
    /// Set how many bit errors to allow. `0` requires the
    /// sync estimate to be error-free.
    ///
    /// When setting this value, we suggest consulting with
    /// the ambiguity function for four bytes of the SAME
    /// preamble: `0xabababab`. Shifting from zero to eight
    /// bits, we find bit errors of:
    ///
    /// ```txt
    /// [0, 24, 8, 24, 8, 24, 8, 24, 0]
    /// ```
    ///
    /// This indicates that invalid offsets are only off by
    /// eight bits. The maximum value of this parameter is
    /// therefore capped at `7`. We suggest a value of
    /// `2` or `3`.
    pub fn with_preamble_max_errors(&mut self, max_err: u32) -> &mut Self {
        self.preamble_max_errors = max_err;
        self
    }

    /// Set adaptive equalizer parameters
    ///
    /// To configure the adaptive equalizer, generate an
    /// [`EqualizerBuilder`](struct.EqualizerBuilder.html)
    /// and provide it to this method.
    pub fn with_adaptive_equalizer(&mut self, eql: &EqualizerBuilder) -> &mut Self {
        self.equalizer = Some(eql.clone());
        self
    }

    /// Disable the adaptive equalizer
    pub fn without_adaptive_equalizer(&mut self) -> &mut Self {
        self.equalizer = None;
        self
    }

    /// Maximum frame start bit errors
    ///
    /// `max_err` is the maximum number of bit errors permitted
    /// when detecting the "beginning of burst" data sequence,
    /// "`ZCZC`" or "`NNNN`." These byte sequences are allowed to
    /// begin a SAME/EAS data transmission, and the framer uses
    /// them to determine when to begin reading data.
    ///
    /// Once the framer detects either of these values, further
    /// changes to the byte synchronization are disallowed until
    /// carrier is dropped.
    ///
    /// We recommend setting this value between zero and two.
    pub fn with_frame_prefix_max_errors(&mut self, max_err: u32) -> &mut Self {
        self.frame_prefix_max_errors = u32::clamp(max_err, 0, 7);
        self
    }

    /// Maximum frame invalid bytes
    ///
    /// The framer will detect the end of transmission after a
    /// frame total of `max_invalid` "invalid" SAME characters
    /// have been received. SAME/EAS uses ASCII bytes, but not
    /// all valid ASCII bytes are valid as SAME characters.
    ///
    /// SAME/EAS transmissions are repeated three times, and it
    /// is often necessary to perform parity correction to
    /// recover a valid message. This makes detecting the end of
    /// a transmission very difficult.
    pub fn with_frame_max_invalid(&mut self, max_invalid: u32) -> &mut Self {
        self.frame_max_invalid_bytes = max_invalid;
        self
    }

    /// Input sampling rate (Hz)
    pub fn input_rate(&self) -> u32 {
        self.input_rate
    }

    /// AGC bandwidth (fraction of input rate)
    pub fn agc_bandwidth(&self) -> f32 {
        self.agc_bandwidth
    }

    /// Timing loop bandwidth (fraction of baud rate)
    ///
    /// Returns both unlocked and locked timing loop bandwidth,
    /// as a fraction of the baud rate.
    pub fn timing_bandwidth(&self) -> (f32, f32) {
        (self.timing_bandwidth_unlocked, self.timing_bandwidth_locked)
    }

    /// Timing maximum deviation (fraction of baud rate)
    pub fn timing_max_deviation(&self) -> f32 {
        self.timing_max_deviation
    }

    /// Squelch power level
    ///
    /// Returns tuple of squelch (`open`, `close`) power level.
    /// A power level of 0.0 disables the power squelch. A power
    /// level of 1.0 requires a completely clean demodulation.
    pub fn squelch_power(&self) -> (f32, f32) {
        (self.squelch_power_open, self.squelch_power_close)
    }

    /// Squelch bandwidth (fraction of baud rate)
    pub fn squelch_bandwidth(&self) -> f32 {
        self.squelch_bandwidth
    }

    /// Maximum preamble sync bit errors
    pub fn preamble_max_errors(&self) -> u32 {
        self.preamble_max_errors
    }

    /// Adaptive equalizer configuration
    pub fn adaptive_equalizer(&self) -> Option<&EqualizerBuilder> {
        self.equalizer.as_ref()
    }

    /// Maximum frame start bit errors
    pub fn frame_prefix_max_errors(&self) -> u32 {
        self.frame_prefix_max_errors
    }

    /// Maximum frame invalid bytes
    pub fn frame_max_invalid(&self) -> u32 {
        self.frame_max_invalid_bytes
    }
}

impl std::default::Default for SameReceiverBuilder {
    fn default() -> Self {
        Self::new(22050)
    }
}

/// Builder for the SAME/EAS decision-feedback equalizer
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct EqualizerBuilder {
    nfeedforward: usize,
    nfeedback: usize,
    relaxation: f32,
    regularization: f32,
}

impl EqualizerBuilder {
    /// New equalizer builder with sensible defaults
    pub fn new() -> Self {
        Self {
            nfeedforward: 6,
            nfeedback: 4,
            relaxation: 0.05f32,
            regularization: 1.0e-6f32,
        }
    }

    /// Set filter order
    ///
    /// The filter order controls how many feedforward taps
    /// and feedback taps the adaptive filter has to work with.
    ///
    /// Higher orders may allow a better fit for channels with
    /// longer or more complicated impulse responses. Using a model
    /// order that is too high risks overfitting, numeric stability
    /// issues, and latency as the filter takes longer to evolve.
    ///
    /// We recommend keeping this relatively low. After all, the
    /// channel *must* be suitable for speech signals for human
    /// listeners.
    ///
    /// Each filter must have at least one tap.
    pub fn with_filter_order(&mut self, nfeedforward: usize, nfeedback: usize) -> &mut Self {
        self.nfeedforward = usize::max(nfeedforward, 1);
        self.nfeedback = usize::clamp(nfeedback, 1, self.nfeedforward);
        self
    }

    /// NLMS relaxation
    ///
    /// Sets the Normalized Least Mean Squares (NLMS) relaxation
    /// parameter. `relaxation` is the the distance to move each
    /// new impulse response estimate to the zero posteriori error
    /// point: 1.0 goes all the way, while 0.0 doesn't move at all.
    /// Sometimes called "mu" or "gain."
    ///
    /// This parameter should be set high enough that the algorithm
    /// converges during the preamble, before the data is read.
    pub fn with_relaxation(&mut self, relaxation: f32) -> &mut Self {
        self.relaxation = f32::clamp(relaxation, 0.0, 1.0);
        self
    }

    /// NLMS regularization
    ///
    /// Sets the Normalized Least Mean Squares (NLMS) regularization
    /// parameter. `regularization` is a main-diagonal weighting
    /// constant that helps keep matrices invertible. Sometimes called
    /// "delta." Set to zero to disable regularization.
    ///
    /// We recommend leaving this parameter alone.
    pub fn with_regularization(&mut self, regularization: f32) -> &mut Self {
        self.regularization = f32::clamp(regularization, 0.0, f32::MAX);
        self
    }

    /// Filter order
    ///
    /// Returns filter order of the `(feedforward, feedback)` portions
    /// of the adaptive filter, respectively.
    pub fn filter_order(&self) -> (usize, usize) {
        (self.nfeedforward, self.nfeedback)
    }

    /// NLMS relaxation
    pub fn relaxation(&self) -> f32 {
        self.relaxation
    }

    /// NLMS regularization
    pub fn regularization(&self) -> f32 {
        self.regularization
    }
}

impl std::default::Default for EqualizerBuilder {
    fn default() -> Self {
        Self::new()
    }
}
