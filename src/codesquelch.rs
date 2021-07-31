//! Access code correlation and squelch
//!
//! The [CodeCorrelator](struct.CodeCorrelator.html)
//! searches the given input symbols for a known sync
//! pattern. The number of bit errors is output. When
//! the number of bit errors is zero, the system is
//! perfectly synchronized. This struct can be used to
//! detect the start of transmission and also to align
//! the system to byte boundaries.
//!
//! The [CodeSquelch](struct.CodeSquelch.html) extends
//! this by buffering symbols and looking for the sync
//! pattern. When sync is found, samples are emitted.
//! The `CodeSquelch` cannot detect loss of sync and
//! must be manually reset.

use arraydeque::ArrayDeque;

#[cfg(not(test))]
use log::info;

#[cfg(test)]
use std::println as info;

/// Access code squelch
///
/// This squelch method suppresses all output samples
/// until a synchronization symbol pattern is detected.
/// The detector makes hard binary decisions and
/// requires a match against the sync word to within a
/// specified maximum number of bit errors.
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
/// This block can only *acquire* the synchronization… it
/// cannot *drop* it. To drop sync, higher-level logic
/// must call [`reset()`](#method.reset).
#[derive(Clone, Debug)]
pub struct CodeSquelch {
    // maximum number of bit errors
    max_errors: u32,

    // correlator
    correlator: CodeCorrelator,

    // sample history
    history: ArrayDeque<[f32; 64], arraydeque::Wrapping>,

    // time since samples were emitted
    sample_clock: Option<u8>,

    // prevent resynchronization
    sync_lock: bool,
}

impl CodeSquelch {
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
    pub fn new(sync_to: u32, max_errors: u32) -> Self {
        let mut out = Self {
            max_errors,
            correlator: CodeCorrelator::new(sync_to),
            history: ArrayDeque::default(),
            sample_clock: None,
            sync_lock: false,
        };
        out.reset();
        out
    }

    /// Process and synchronize input
    ///
    /// The `CodeSquelch` expects *TWO* samples per symbol at the
    /// input. The input must already be aligned to the symbol
    /// clock: `input[0]` must be a zero, and `input[1]` must be
    /// a symbol estimate. This is, coincidentally, just how
    /// [`TimingLoop`](../symsync/struct.TimingLoop.html) outputs
    /// them.
    ///
    /// If the output is `None`, the sync has not yet been found.
    ///
    /// If `Some`, the output is a tuple of `(samples, sync)`,
    /// where `samples` is an array of 16 samples (8 symbols).
    ///
    /// The `sync` flag is set to true if the first symbol in the
    /// output array is the first sample of the `sync_to` sync word.
    /// The `sync` output may be used to put an equalizer into
    /// training mode against the sync word.
    ///
    /// This method may panic if `input` does not contain
    /// exactly two samples (and will panic in debug mode).
    pub fn input(&mut self, input: &[f32]) -> Option<([f32; Self::OUTPUT_LENGTH], bool)> {
        // append to history and correlator
        assert_eq!(Self::INPUT_LENGTH, input.len());
        self.history.push_back(input[0]);
        self.history.push_back(input[1]);
        let err = self.correlator.search(input[1]);

        if !self.history.is_full() {
            // wait for buffer to fill
            return None;
        }

        // we find the sync if we are allowed to and the
        // err count is low enough
        let new_sync = !self.sync_lock && err <= self.max_errors;
        if new_sync {
            self.sample_clock = match self.sample_clock {
                None => {
                    info!("acquired byte synchronization with {} errors", err);
                    Some(0)
                }
                Some(n) => {
                    info!(
                        "adjust byte synchronization by +{} symbols with {} errors",
                        8 - n,
                        err
                    );
                    Some(0)
                }
            }
        }

        // if the output clock is byte-aligned, output the last
        // byte's worth of samples
        match self.sample_clock {
            None => None,
            Some(0) => {
                let mut out = [0.0f32; Self::OUTPUT_LENGTH];
                for (o, h) in out
                    .iter_mut()
                    .zip(self.history.iter().take(Self::OUTPUT_LENGTH))
                {
                    *o = *h;
                }
                self.sample_clock = Some(1);
                Some((out, new_sync))
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
    /// on a [`reset()`](#method.reset).
    pub fn lock(&mut self, lock: bool) {
        self.sync_lock = lock;
    }

    /// Reset to zero initial conditions
    ///
    /// The sync is dropped and must be re-acquired before
    /// additional data bytes can be output.
    pub fn reset(&mut self) {
        self.correlator.reset();
        self.history.clear();
        self.sample_clock = None;
        self.sync_lock = false;
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
    /// When unlocked, the `CodeSquelch` will resynchronize
    /// itself *any time* it detects the sync sequence. If your
    /// frame data might contain the sync sequence, this could
    /// be a problem. When locked, the `CodeSquelch` will never
    /// resynchronize once sync is acquired.
    pub fn is_locked(&self) -> bool {
        self.sync_lock
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

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_simple_sync() {
        const BYTES: &[u8] = &[0xAB, 0xAB, 0xAB, 0xAB, 0x21];
        let insamp = bytes_to_samples(BYTES, 2);

        let mut squelch = CodeSquelch::new(crate::waveform::PREAMBLE_SYNC_WORD, 0);
        assert!(!squelch.is_sync());

        let mut align_idx = 0; // sync sequence begins at 0 samples in insamp
        for (chunk, inp) in insamp.chunks(2).enumerate() {
            let lock = squelch.input(inp);
            match lock {
                Some((outsamp, is_sync)) => {
                    // only get one sync pulse
                    if is_sync {
                        assert_eq!(chunk, 31);
                        assert_eq!(squelch.correlator.data, 0xabababab);
                    }
                    assert!(!is_sync || chunk == 31);

                    // outsamp is aligned with insamp
                    assert_eq!(&outsamp, &insamp[align_idx..align_idx + 16]);
                    align_idx += 16;
                }
                _ => {}
            }
        }

        assert!(squelch.is_sync());
        assert_eq!(align_idx, 32);
        squelch.reset();
        assert!(!squelch.is_sync());
    }

    #[test]
    fn test_sync_with_error() {
        const BYTES: &[u8] = &[0xF0, 0x0B, 0xA9, 0xAB, 0xAB, 0xAB, 0x21];
        let insamp = bytes_to_samples(BYTES, 2);

        let mut squelch = CodeSquelch::new(crate::waveform::PREAMBLE_SYNC_WORD, 1);
        assert!(!squelch.is_sync());

        let mut align_idx = 32; // sync sequence begins at 32 samples in insamp
        for (chunk, inp) in insamp.chunks(2).enumerate() {
            let lock = squelch.input(inp);
            match lock {
                Some((outsamp, is_sync)) => {
                    assert!(!is_sync || 47 == chunk);
                    assert_eq!(&outsamp, &insamp[align_idx..align_idx + 16]);
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
        let mut squelch = CodeSquelch::new(crate::waveform::PREAMBLE_SYNC_WORD, 3);
        assert!(!squelch.is_sync());

        let mut align_idx = 32; // sync sequence begins at 32 samples in insamp
        for (chunk, inp) in insamp.chunks(2).enumerate() {
            let lock = squelch.input(inp);
            match lock {
                Some((outsamp, _is_sync)) => {
                    if chunk == 47 {
                        assert_eq!(&outsamp, &insamp[align_idx..align_idx + 16]);
                        align_idx += 16;
                        found_later = true;
                    } else {
                        found_early = true;
                        std::mem::drop(outsamp);
                    }
                }
                _ => {}
            }
        }

        assert!(squelch.is_sync());
        assert!(found_later);
        assert!(found_early);
    }
}
