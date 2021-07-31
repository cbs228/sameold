//! Access code correlation and squelch
//!
//! The [`CodeCorrelator`]
//! searches the given input symbols for a known sync
//! pattern. The number of bit errors is output. When
//! the number of bit errors is zero, the system is
//! perfectly synchronized. This struct can be used to
//! detect the start of transmission and also to align
//! the system to byte boundaries.
//!
//! The [`CodeAndPowerSquelch`]
//! extends this by buffering symbols and looking for
//! the sync pattern. When sync is found, samples are
//! emitted. The `CodeAndPowerSquelch` also enforces a
//! minimum signal power.

use std::default::Default;

use arraydeque::ArrayDeque;

#[cfg(not(test))]
use log::debug;

#[cfg(test)]
use std::println as debug;

/// Squelch state
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SquelchState {
    /// Reading data
    ///
    /// No state change. The squelch is synchronized and reading
    /// data.
    Reading,

    /// Acquired symbol synchronization
    ///
    /// The synchronization has been acquired and/or adjusted.
    Acquired,

    /// Dropped symbol synchronization
    ///
    /// No further [`SquelchOut`] will
    /// be output until sync is acquired.
    Dropped,
}

/// Squelch output
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct SquelchOut {
    /// Synchronized data samples
    ///
    /// Synchronized samples at *twice* the symbol rate. The array
    /// is aligned to data byte boundaries and contains exactly
    /// one byte of symbols (16 samples).
    ///
    /// ```txt
    /// [0.0, 1.0, 0.0, … … … , 0.0, -1.0]
    ///       |                       |
    ///       |                       + most recent symbol
    ///       |
    ///       + least recent symbol
    /// ```
    ///
    /// The most recent symbols are to the right of the array.
    pub samples: [f32; CodeAndPowerSquelch::OUTPUT_LENGTH],

    /// Lifetime total symbols received
    ///
    /// Counts the lifetime total number of symbols *received*
    /// by the `CodeAndPowerSquelch`. This is *not* the same as
    /// the number of symbols emitted by the squelch. This field
    /// is useful as a measure of time.
    pub symbol_counter: u64,

    /// Symbol power estimate
    ///
    /// A smoothed estimate of the symbol power, in linear power
    /// power (amplitude-squared) units. This value is the power
    /// estimate as of the last symbol in `samples`.
    pub power: f32,

    /// Reports squelch state
    ///
    /// If the sync has been acquired or adjusted, reports
    /// `SquelchState::Acquired`. If the sync is dropped and
    /// this is the last output, reports `SquelchState::Dropped`.
    pub state: SquelchState,
}

/// Access code and power squelch
///
/// This squelch method suppresses all output samples
/// until a synchronization symbol pattern is detected
/// at a specified minimum power level. The detector
/// makes hard binary decisions and requires a match
/// against the sync word to within a specified maximum
/// number of bit errors.
///
/// This block requires that symbols be transmitted
/// least-significant bit (LSb) first.
///
/// When the synchronization is acquired, the input
/// *samples* are output. Samples are output verbatim;
/// the decisions made by the correlator are *not* output.
/// This permits the samples to be equalized or otherwise
/// be further processed prior to decision-making.
///
/// The synchronization may be adjusted after it is
/// acquired. To lock the synchronization, use
/// [`lock(true)`](#method.lock). Locking the
/// synchronization prevents sync-like sequences in the
/// frame data from causing erroneous resync.
///
/// Output samples are always aligned to byte boundaries,
/// which makes it easy to assemble them into bytes.
///
/// This block imposes a delay of 32 *symbols* (64 samples)
/// and will always output 16 samples at a time when output
/// is available. This is, conveniently, one byte's worth
/// of samples.
///
/// This block will drop sync ("close the squelch") if the
/// received power level drops below the low-water mark. You
/// may also drop sync with the [`end()`](#method.end) method.
#[derive(Clone, Debug)]
pub struct CodeAndPowerSquelch {
    // maximum number of bit errors
    max_errors: u32,

    // power required to open the squelch (linear amplitude^2)
    power_open: f32,

    // power required to keep the squelch open (linear amplitude^2)
    power_close: f32,

    // correlator
    correlator: CodeCorrelator,

    // power tracker
    power_track: PowerTracker,

    // sample history
    sample_history: ArrayDeque<[f32; 64], arraydeque::Wrapping>,

    // power threshold history
    power_history: ArrayDeque<[bool; 32], arraydeque::Wrapping>,

    // lifetime total count of symbols received
    symbol_counter: u64,

    // time since samples were emitted
    sample_clock: Option<u8>,

    // prevent resynchronization
    sync_lock: bool,
}

impl CodeAndPowerSquelch {
    /// Input length, in samples
    pub const INPUT_LENGTH: usize = 2;

    /// Output length, in samples
    pub const OUTPUT_LENGTH: usize = 16;

    /// Create squelch
    ///
    /// The synchronizer will look for the bit pattern given in the
    /// `sync_to` word with no more than `max_errors` errors. When
    /// setting `max_errors`, be mindful of the correlation sidelobes
    /// of your synchronization code. Many synchronization waveforms
    /// have ambiguities at nearby bit shifts, and it is important
    /// not to admit these.
    ///
    /// The `CodeAndPowerSquelch` also tracks the power level of the
    /// demodulated symbols and can enforce a minimum power level.
    /// Power is estimated as the square of the symbol values. (It is
    /// expected that the input to the demodulator is normalized with
    /// an AGC.) The symbol power must reach at least `power_open` to
    /// open the squelch, and the squelch will close if the power falls
    /// below `power_close`. Set these values to zero to disable the
    /// power detector and use the code only. In general, we recommend
    /// `power_open` >= `power_close`.
    ///
    /// The power estimates are smoothed with a single-pole IIR filter
    /// of bandwidth `power_track_bandwidth`, which is expressed as a
    /// fraction of the symbol rate.
    pub fn new(
        sync_to: u32,
        max_errors: u32,
        power_open: f32,
        power_close: f32,
        power_track_bandwidth: f32,
    ) -> Self {
        let mut out = Self {
            max_errors,
            power_open,
            power_close: f32::min(power_close, power_open),
            correlator: CodeCorrelator::new(sync_to),
            power_track: PowerTracker::new(power_track_bandwidth),
            sample_history: ArrayDeque::default(),
            power_history: ArrayDeque::default(),
            symbol_counter: 0,
            sample_clock: None,
            sync_lock: false,
        };
        out.reset();
        out
    }

    /// Process and synchronize input
    ///
    /// The `CodeAndPowerSquelch` expects *TWO* samples per symbol
    /// at the input. The input must already be aligned to the symbol
    /// clock: `input[0]` must be a zero, and `input[1]` must be
    /// a symbol estimate. This is, coincidentally, just how
    /// [`TimingLoop`](crate::symsync::TimingLoop) outputs
    /// them.
    ///
    /// If the output is `None`, the sync has not yet been found.
    ///
    /// If the output is `Some`, the sync has been acquired.
    /// Synchronized samples are output in [`SquelchOut`].
    ///
    /// This method may panic if `input` does not contain
    /// exactly two samples (and will panic in debug mode).
    pub fn input(&mut self, input: &[f32]) -> Option<SquelchOut> {
        // append to history and correlator
        assert_eq!(Self::INPUT_LENGTH, input.len());
        self.sample_history.push_back(input[0]);
        self.sample_history.push_back(input[1]);
        let err = self.correlator.search(input[1]);
        let pwr = self.power_track.track(input[1]);
        self.power_history.push_back(pwr >= self.power_close);
        self.symbol_counter += 1;

        if !self.sample_history.is_full() {
            // wait for buffer to fill
            return None;
        }

        let new_sync = !self.sync_lock && err <= self.max_errors && pwr >= self.power_open;

        let report_state = if new_sync {
            self.sample_clock = match self.sample_clock {
                None => {
                    debug!(
                        "squelch: acquired byte sync: {} errors, power {:.3}",
                        err, pwr
                    );
                    Some(0)
                }
                Some(0) => {
                    // sync is re-affirmed
                    Some(0)
                }
                Some(n) => {
                    debug!(
                        "squelch: adjust byte sync by +{} symbols with {} errors, power {:.3}",
                        8 - n,
                        err,
                        pwr
                    );
                    Some(0)
                }
            };
            SquelchState::Acquired
        } else if self.is_sync() && !self.power_history.front().expect("bad power history") {
            debug!(
                "squelch: lost sync: symbol power {:.3} below threshold",
                pwr
            );
            self.end();

            let mut out = SquelchOut::default();
            out.symbol_counter = self.symbol_counter;
            out.power = pwr;
            out.state = SquelchState::Dropped;

            return Some(out);
        } else {
            SquelchState::Reading
        };

        // if the output clock is byte-aligned, output the last
        // byte's worth of samples
        match self.sample_clock {
            None => None,
            Some(0) => {
                self.sample_clock = Some(1);

                let mut out = SquelchOut::default();
                for (o, h) in out
                    .samples
                    .iter_mut()
                    .zip(self.sample_history.iter().take(Self::OUTPUT_LENGTH))
                {
                    *o = *h;
                }
                out.symbol_counter = self.symbol_counter;
                out.power = pwr;
                out.state = report_state;
                Some(out)
            }
            Some(ref mut clk) => {
                *clk = (*clk + 1) % 8;
                None
            }
        }
    }

    /// Lock the synchronization
    ///
    /// If `lock` is true, prevent the synchronization from
    /// changing once it is acquired. This setting is cleared
    /// on a call to [`end()`](#method.end).
    pub fn lock(&mut self, lock: bool) {
        self.sync_lock = lock;
    }

    /// Reset to zero initial conditions
    ///
    /// If all you want to do is drop synchronization, you
    /// probably want to use the
    /// [`end()`](#method.end) instead.
    pub fn reset(&mut self) {
        self.end();
        self.correlator.reset();
        self.sample_history.clear();
        self.power_track.reset();
        self.power_history.clear();
        self.symbol_counter = 0;
    }

    /// Drop synchronization, inhibiting further output
    ///
    /// Signals the `CodeAndPowerSquelch` that the frame or
    /// byte synchronization has been lost. The squelch is
    /// immediately closed and the sync is dropped.
    /// [`input()`](#method.input) will produce no further
    /// outputs until synchronization is reacquired.
    pub fn end(&mut self) {
        self.sync_lock = false;
        self.sample_clock = None;
    }

    /// Lifetime count of received symbols
    ///
    /// Reports a total count of all *symbols* received since
    /// the last [`reset()`](#method.reset). This value is in
    /// symbols and not in samples. All received samples are
    /// counted, including those which are suppressed by the
    /// squelch mechanism.
    pub fn symbol_count(&self) -> u64 {
        self.symbol_counter
    }

    /// Power tracker state
    ///
    /// Returns the smoothed *symbol power* estimate as of
    /// the last call to `input()`.
    ///
    /// Since the input signal is typically normalized to
    /// an amplitude of 1.0, these values will *typically*
    /// range from 0.0 (no signal at all) to 1.0 (maximum
    /// symbol power). Values above 1.0 are possible,
    /// however.
    pub fn power(&self) -> f32 {
        self.power_track.power
    }

    /// Is the squelch synchronized?
    ///
    /// If the system is synchronized, synchronized samples
    /// will be emitted.
    pub fn is_sync(&self) -> bool {
        self.sample_clock.is_some()
    }

    /// Is the synchronization locked?
    ///
    /// When unlocked, the `CodeAndPowerSquelch` will resynchronize
    /// itself *any time* it detects the sync sequence. If your
    /// frame data might contain the sync sequence, this could
    /// be a problem. When locked, the `CodeAndPowerSquelch` will
    /// never resynchronize once sync is acquired.
    #[allow(dead_code)]
    pub fn is_locked(&self) -> bool {
        self.sync_lock
    }
}

impl Default for SquelchState {
    fn default() -> SquelchState {
        SquelchState::Reading
    }
}

/// Access code correlator
///
/// This synchronizer searches for a `sync_to` word.
/// The word must be transmitted least significant bit
/// first.
#[derive(Clone, Debug)]
pub struct CodeCorrelator {
    // sync word
    sync_to: u32,

    // current byte created from input
    data: u32,
}

impl CodeCorrelator {
    /// Create synchronizer
    ///
    /// The synchronizer looks for the byte `sync_to` and
    /// will lock on if the number of bit errors is zero.
    pub fn new(sync_to: u32) -> Self {
        Self { sync_to, data: 0 }
    }

    /// Process input symbol
    ///
    /// The correlator makes a hard binary decision on the
    /// given symbol `sym`, searches for the sync sequence,
    /// and returns the number of bit errors.
    pub fn search(&mut self, sym: f32) -> u32 {
        // shift bit onto data from the left
        let bit = (sym >= 0.0f32) as u32;
        self.data = self.data >> 1;
        self.data |= bit << 31;

        num_bit_errors(self.sync_to, self.data)
    }

    /// Reset to zero initial conditions
    ///
    /// The sync is dropped and must be re-acquired before
    /// additional data bytes can be output.
    pub fn reset(&mut self) {
        self.data = 0;
    }
}

// Test for sync
#[inline]
fn num_bit_errors(sync_to: u32, data: u32) -> u32 {
    // any 1 bits are bit errors here
    let err = sync_to ^ data;
    err.count_ones()
}

// Tracks received signal power
//
// The power tracker tracks the power level (in linear
// amplitude^2 units) of the received symbols. The power
// estimate is smoothed by a single-pole IIR filter with
// a specified `bandwidth`.
#[derive(Clone, Debug)]
struct PowerTracker {
    bandwidth: f32,
    power: f32,
}

impl PowerTracker {
    /// New power tracker
    ///
    /// The tracker smooths its estimates with the given
    /// `bandwidth`, which is specified by as a fraction
    /// of the baud rate.
    pub fn new(bandwidth: f32) -> Self {
        Self {
            bandwidth: f32::clamp(bandwidth, 0.0f32, 1.0f32),
            power: 0.0f32,
        }
    }

    /// Reset to zero initial conditions
    pub fn reset(&mut self) {
        self.power = 0.0f32;
    }

    /// Track power
    ///
    /// Accepts a soft symbol estimate in `sym` and calculates
    /// a smoothed power estimate. The power estimate is output
    /// in units of amplitude^2.
    #[inline]
    pub fn track(&mut self, sym: f32) -> f32 {
        let pwr = sym * sym;
        self.power += (pwr - self.power) * self.bandwidth;
        self.power = self.power.max(0.0f32);
        self.power
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use assert_approx_eq::assert_approx_eq;

    use crate::waveform::{bytes_to_samples, bytes_to_symbols};

    #[test]
    fn test_num_bit_errors() {
        let sync_to = crate::waveform::PREAMBLE_SYNC_WORD;
        let one_error = sync_to | 0x40u32;
        let another_error = 0xa9ababab;

        assert_eq!(0, num_bit_errors(sync_to, sync_to));
        assert_eq!(1, num_bit_errors(sync_to, one_error));
        assert_eq!(1, num_bit_errors(sync_to, another_error));
    }

    #[test]
    fn test_codecorr() {
        const BYTES: &[u8] = &[0xAB, 0xAB, 0xAB, 0xAB, 0x21];
        let mut syms = bytes_to_symbols(BYTES);

        let mut uut = CodeCorrelator::new(crate::waveform::PREAMBLE_SYNC_WORD);

        // correlation reaches minimum error at 32 bits
        let out: Vec<u32> = syms.iter().map(|s| uut.search(*s)).collect();
        for (i, err) in out.iter().enumerate() {
            match i {
                31 => assert_eq!(*err, 0),
                _ => assert!(*err > 0),
            }
        }

        // flip one of the bits. now the minimum error is one.
        syms[19] = -syms[19];

        let out: Vec<u32> = syms.iter().map(|s| uut.search(*s)).collect();
        for (i, err) in out.iter().enumerate() {
            match i {
                31 => assert_eq!(*err, 1),
                _ => assert!(*err >= 1),
            }
        }
    }

    #[test]
    fn test_power_tracker() {
        let mut ptrack = PowerTracker::new(1.0f32);
        ptrack.track(1.0f32);
        ptrack.bandwidth = 0.5f32;
        assert_approx_eq!(0.62500f32, ptrack.track(-0.5f32));

        ptrack.power = 1.0f32;
        for _i in 0..16 {
            ptrack.track(1.0f32);
        }
        assert_approx_eq!(1.0f32, ptrack.power);
        assert_approx_eq!(1.0f32, ptrack.track(1.0f32));
    }

    #[test]
    fn test_simple_sync() {
        const BYTES: &[u8] = &[0xAB, 0xAB, 0xAB, 0xAB, 0x21];
        let insamp = bytes_to_samples(BYTES, 2);

        let mut squelch =
            CodeAndPowerSquelch::new(crate::waveform::PREAMBLE_SYNC_WORD, 0, 0.0, 0.0, 0.1);
        assert!(!squelch.is_sync());

        let mut align_idx = 0; // sync sequence begins at 0 samples in insamp
        for (chunk, inp) in insamp.chunks(2).enumerate() {
            let lock = squelch.input(inp);
            match lock {
                Some(out) => {
                    // only get one sync pulse
                    if out.state == SquelchState::Acquired {
                        assert_eq!(chunk, 31);
                        assert_eq!(squelch.correlator.data, 0xabababab);
                    }
                    assert!(out.state != SquelchState::Acquired || chunk == 31);

                    // outsamp is aligned with insamp
                    assert_eq!(&out.samples, &insamp[align_idx..align_idx + 16]);
                    align_idx += 16;

                    assert_eq!(out.symbol_counter - 1, chunk as u64);
                }
                _ => {}
            }
        }

        assert!(squelch.is_sync());
        assert_eq!(align_idx, 32);
        squelch.end();
        assert!(!squelch.is_sync());
    }

    #[test]
    fn test_sync_with_error() {
        const BYTES: &[u8] = &[0xF0, 0x0B, 0xA9, 0xAB, 0xAB, 0xAB, 0x21];
        let insamp = bytes_to_samples(BYTES, 2);

        let mut squelch =
            CodeAndPowerSquelch::new(crate::waveform::PREAMBLE_SYNC_WORD, 1, 0.0, 0.0, 0.1);
        assert!(!squelch.is_sync());

        let mut align_idx = 32; // sync sequence begins at 32 samples in insamp
        for (chunk, inp) in insamp.chunks(2).enumerate() {
            let lock = squelch.input(inp);
            match lock {
                Some(out) => {
                    assert!(out.state != SquelchState::Acquired || 47 == chunk);
                    assert_eq!(&out.samples, &insamp[align_idx..align_idx + 16]);
                    align_idx += 16;
                }
                _ => {}
            }
        }

        assert!(squelch.is_sync());
    }

    // here, we find an erroneous sync early on but then acquire a better sync
    #[test]
    fn test_sync_with_lots_of_errors() {
        const BYTES: &[u8] = &[0xAB, 0x0B, 0xA9, 0xAB, 0xAB, 0xAA, 0x21];
        let insamp = bytes_to_samples(BYTES, 2);

        let mut found_early = false;
        let mut found_later = false;
        let mut squelch =
            CodeAndPowerSquelch::new(crate::waveform::PREAMBLE_SYNC_WORD, 3, 0.0, 0.0, 0.1);
        assert!(!squelch.is_sync());

        let mut align_idx = 32; // sync sequence begins at 32 samples in insamp
        for (chunk, inp) in insamp.chunks(2).enumerate() {
            let lock = squelch.input(inp);
            match lock {
                Some(out) => {
                    if chunk == 47 {
                        assert_eq!(&out.samples, &insamp[align_idx..align_idx + 16]);
                        align_idx += 16;
                        found_later = true;
                    } else {
                        found_early = true;
                    }
                }
                _ => {}
            }
        }

        assert!(squelch.is_sync());
        assert!(found_later);
        assert!(found_early);
    }

    #[test]
    fn test_power_detection() {
        const BYTES: &[u8] = &[0xF0, 0x0B, 0xA9, 0xAB, 0xAB, 0xAB, 0x21];
        let insamp = bytes_to_samples(BYTES, 2);

        // we open the squelch
        let mut squelch =
            CodeAndPowerSquelch::new(crate::waveform::PREAMBLE_SYNC_WORD, 1, 0.9, 0.5, 0.1);
        for inp in insamp.chunks(2) {
            let _ = squelch.input(inp);
        }
        assert!(squelch.is_sync());

        // now append a bunch of zeroes and watch it close
        let mut last = SquelchOut::default();
        for _i in 0..40 {
            if let Some(out) = squelch.input(&[0.0f32, 0.0f32]) {
                last = out;
            }
        }

        assert!(!squelch.is_sync());
        assert!(last.state == SquelchState::Dropped);
    }
}
