//! Full receiver chain

#[cfg(not(test))]
use log::{info, trace, warn};

#[cfg(test)]
use std::println as trace;
#[cfg(test)]
use std::println as info;
#[cfg(test)]
use std::println as warn;

use std::convert::From;
use std::iter::{IntoIterator, Iterator};

use crate::agc::Agc;
use crate::builder::{EqualizerBuilder, SameReceiverBuilder};
use crate::codesquelch::{CodeAndPowerSquelch, SquelchState};
use crate::demod::{Demod, FskDemod};
use crate::equalize::Equalizer;
use crate::framing::{FrameOut, Framer};
use crate::message::Message;
use crate::symsync::TimingLoop;

/// A complete SAME/EAS receiver chain
///
/// The receive chain takes `f32` audio samples and
/// performs the following operations:
///
/// 1. Automatic gain control
/// 2. Demodulation and down-sampling to two samples
///    per symbol, governed by a zero-crossing detector
///    timing recovery loop.
/// 3. Access code correlation and squelch. The code
///    correlator also synchronizes to byte boundaries.
/// 4. (Optional) Adaptive decision feedback equalization
/// 5. Framing and message decoding
///
/// To create the receiver, first create its Builder:
///
/// ```
/// use sameold::SameReceiverBuilder;
///
/// let mut builder = SameReceiverBuilder::default();
/// let receiver = builder.build();
/// assert_eq!(receiver.input_rate(), 22050);
/// ```
///
/// See [module documentation](index.html) for details.
#[derive(Clone, Debug)]
pub struct SameReceiver {
    agc: Agc,
    demod: FskDemod,
    symsync: TimingLoop,
    squelch: CodeAndPowerSquelch,
    equalizer: Equalizer,
    framer: Framer,
    timing_bandwidth_unlocked: f32,
    timing_bandwidth_locked: f32,
    input_rate: u32,
    input_sample_counter: u64,
    last_state: FrameOut,
    ted_sample_clock: u32,
    samples_until_next_ted: f32,
    force_eom_at_sample: Option<u64>,
}

impl SameReceiver {
    /// Receive SAME messages from a source of audio
    ///
    /// Bind an iterator which will consume the `input` and
    /// produce SAME [`FrameOut`](enum.FrameOut.html) events,
    /// which include:
    ///
    /// * notifications about acquired and dropped carrier,
    /// * attempts to frame messages; and
    /// * successful framed messages
    ///
    /// The `input` must be f32 PCM mono audio at
    /// the [`input_rate()`](#method.input_rate) for this
    /// receiver. Sound cards commonly output audio samples
    /// in `i16` format. You must perform the conversion to
    /// floating-point yourself, if needed. It is unnecessary
    /// to scale the converted values; our AGC algorithm will
    /// take care of that.
    ///
    /// The iterator will consume as many samples of `input`
    /// that are required to produce the next event. It will
    /// return `None` if the input is exhausted and there
    /// are no new events.
    #[must_use = "iterators are lazy and do nothing unless consumed"]
    pub fn iter<'rx, I, T>(&'rx mut self, input: I) -> SourceIter<'rx, T>
    where
        I: IntoIterator<Item = f32> + IntoIterator<IntoIter = T>,
        T: Iterator<Item = f32>,
    {
        SourceIter {
            source: input.into_iter(),
            receiver: self,
        }
    }

    /// Input sampling rate
    ///
    /// Returns sampling rate expected by the
    /// [`process()`](#method.process) method.
    pub fn input_rate(&self) -> u32 {
        self.input_rate
    }

    /// Lifetime total input sample counter
    ///
    /// Reports the lifetime total of input samples which
    /// have been processed.
    pub fn input_sample_counter(&self) -> u64 {
        self.input_sample_counter
    }

    /// Clear all DSP states and reset to zero initial conditions
    ///
    /// All buffers and states are cleared.
    pub fn reset(&mut self) {
        self.agc.reset();
        self.demod.reset();
        self.symsync.reset();
        self.squelch.reset();
        self.equalizer.reset();
        self.framer.reset();
        self.input_sample_counter = 0;
        self.last_state = FrameOut::NoCarrier;
        self.ted_sample_clock = 0;
        self.samples_until_next_ted = self.symsync.samples_per_ted();
        self.force_eom_at_sample = None;
    }

    /// Flush the DSP buffers and emit any leftover messages
    ///
    /// The DSP algorithms impose delay on the input. When
    /// processing recorded audio that has been "close cut"
    /// to the extents of a message, the `SameReceiver` might
    /// not emit the message. This is because not all of the
    /// data samples from the file have made their way through
    /// the entire system.
    ///
    /// This method flushes the input with an adequate number
    /// of zeros to ensure all buffered samples have been
    /// processed. Returns the last `Message` generated, if
    /// any.
    ///
    /// You probably want to [`reset()`](#method.reset) after
    /// calling this method.
    pub fn flush(&mut self) -> Option<Message> {
        let two_seconds_of_zeros = std::iter::repeat(0.0f32)
            .zip(0..self.input_rate * 2)
            .map(|(sa, _)| sa);
        let mut out = None;
        for evt in self.iter(two_seconds_of_zeros) {
            match evt {
                FrameOut::Ready(Ok(msg)) => out = Some(msg),
                _ => {}
            }
        }
        out
    }

    // Process a single high-rate sample
    //
    // Accepts a signed 16-bit PCM audio sample as `input`
    // and processes it for SAME messages. If the system's
    // state changes, a [`FrameOut`](enum.FrameOut.html)
    // is emitted.
    #[inline]
    fn process_high_rate(&mut self, input: f32) -> Option<FrameOut> {
        // high-rate processing: agc and push onto demodulator's buffer
        let sa = self.agc.input(input);
        self.demod.push(&[sa]);
        self.ted_sample_clock += 1;
        self.input_sample_counter = self.input_sample_counter.wrapping_add(1);

        // compute time until we sample for the timing error detector
        // positive → before time, negative → after time
        let clock_remaining_sa = self.samples_until_next_ted - self.ted_sample_clock as f32;
        if clock_remaining_sa <= 0.0f32 || clock_remaining_sa.abs() < 0.5f32 {
            self.ted_sample_clock = 0;
            let out = self.process_low_rate(clock_remaining_sa)?;
            if out != self.last_state {
                match &out {
                    FrameOut::Reading => {
                        // prevent sync-like sequences in the message data
                        // from changing the sync
                        self.squelch.lock(true);
                    }
                    FrameOut::NoCarrier => self.end(),
                    FrameOut::Ready(Ok(Message::StartOfMessage(_))) => {
                        // set timeout for maximum length of voice message
                        self.end();
                        self.force_eom_at_sample = Some(
                            self.input_sample_counter
                                + Self::MAX_MESSAGE_DURATION_SECS * self.input_rate as u64,
                        );
                    }
                    FrameOut::Ready(Ok(Message::EndOfMessage)) => {
                        self.end();
                        self.force_eom_at_sample = None
                    }
                    FrameOut::Ready(_) => {
                        self.end();
                    }
                    _ => {}
                }
                self.last_state = out.clone();
                Some(out)
            } else {
                // No change
                None
            }
        } else if let Some(timeout) = self.force_eom_at_sample {
            // Handle timeout
            if self.input_sample_counter > timeout {
                warn!(
                    "voice message timeout ({} s) exceeded; forcing end-of-message now",
                    Self::MAX_MESSAGE_DURATION_SECS
                );
                self.force_eom_at_sample = None;
                Some(FrameOut::Ready(Ok(Message::EndOfMessage)))
            } else {
                None
            }
        } else {
            None
        }
    }

    // Low-rate DSP at two samples per symbol
    //
    // `clock_remaining_sa` is the error between the current
    // high-rate sample time (must be integer) and the
    // commanded sample time (may be fractional), in
    // high-rate samples.
    // * positive → before time
    // * negative → after time
    fn process_low_rate(&mut self, clock_remaining_sa: f32) -> Option<FrameOut> {
        // 1. demod from window
        let sa_low = self.demod.demod();

        // 2. symbol timing error detection
        let sync_out = self.symsync.input(sa_low, clock_remaining_sa);
        self.samples_until_next_ted = sync_out.0;
        let bit_samples = sync_out.1?;

        if self.squelch.symbol_count() % Self::TRACE_LOG_INTERVAL_SYMS == 0 {
            trace!(
                "[{:<14}]: signal magnitude {:0.1}, symbol power: {:0.2}",
                self.input_sample_counter(),
                1.0f32 / self.agc.gain(),
                self.squelch.power()
            );
        }

        // 3. power and access code correlation squelch
        let squelch_out = self.squelch.input(&bit_samples.data)?;
        let is_resync = match &squelch_out.state {
            SquelchState::Acquired => {
                // when byte sync is achieved, lock down the AGC
                // and bit synchronizer. Put the equalizer
                // in training mode
                self.agc.lock(true);
                self.symsync
                    .set_loop_bandwidth(self.timing_bandwidth_locked);
                self.equalizer
                    .train()
                    .expect("equalizer missing training sequence");
                true
            }
            SquelchState::Reading => false,
            SquelchState::Dropped => {
                // end the framer now
                return Some(self.framer.end(squelch_out.symbol_counter));
            }
        };

        // // 4. adaptive equalization
        let (byte_est, _) = self.equalizer.input(&squelch_out.samples);

        // 5. framing
        Some(
            self.framer
                .input(byte_est, squelch_out.symbol_counter, is_resync),
        )
    }

    // Handle "no carrier" / loss of signal
    //
    // Resets all locked DSPs, including the squelch.
    fn end(&mut self) {
        self.agc.lock(false);
        self.squelch.end();
        self.equalizer.reset();
        self.symsync
            .set_loop_bandwidth(self.timing_bandwidth_unlocked);
        self.symsync.reset();
    }

    // Maximum length of a SAME/EAS voice message
    //
    // This is the maximum length of the analog voice message, and
    // *NOT* the length of the digital data
    const MAX_MESSAGE_DURATION_SECS: u64 = 135;

    // Print trace-level messages about once per second
    const TRACE_LOG_INTERVAL_SYMS: u64 = 520;
}

impl From<&SameReceiverBuilder> for SameReceiver {
    /// Create the SAME Receiver from its Builder
    fn from(cfg: &SameReceiverBuilder) -> Self {
        let input_rate = cfg.input_rate();
        let sps = crate::waveform::samples_per_symbol(input_rate);
        let (timing_bandwidth_unlocked, timing_bandwidth_locked) = cfg.timing_bandwidth();
        let (power_open, power_close) = cfg.squelch_power();

        let agc = Agc::new(
            cfg.agc_bandwidth() * sps / input_rate as f32,
            cfg.agc_gain_limits()[0],
            cfg.agc_gain_limits()[1],
        );
        let demod = FskDemod::new_from_same(cfg.input_rate());
        let symsync = TimingLoop::new(sps, timing_bandwidth_unlocked, cfg.timing_max_deviation());
        let code_squelch = CodeAndPowerSquelch::new(
            crate::waveform::PREAMBLE_SYNC_WORD,
            cfg.preamble_max_errors(),
            power_open,
            power_close,
            cfg.squelch_bandwidth(),
        );
        let eqcfg = match cfg.adaptive_equalizer() {
            Some(eqcfg) => *eqcfg,
            None => disabled_equalizer(),
        };
        let equalizer = Equalizer::new(
            eqcfg.filter_order().0,
            eqcfg.filter_order().1,
            eqcfg.relaxation(),
            eqcfg.regularization(),
            Some(crate::waveform::PREAMBLE_SYNC_WORD),
        );
        let framer = Framer::new(cfg.frame_prefix_max_errors(), cfg.frame_max_invalid());

        let samples_until_next_ted = symsync.samples_per_ted();

        Self {
            agc,
            demod,
            symsync,
            squelch: code_squelch,
            equalizer,
            framer,
            timing_bandwidth_unlocked,
            timing_bandwidth_locked,
            input_rate,
            input_sample_counter: 0,
            last_state: FrameOut::NoCarrier,
            ted_sample_clock: 0,
            samples_until_next_ted,
            force_eom_at_sample: None,
        }
    }
}

/// Sample source iterator
///
/// This iterator is bound to a source of mono f32 PCM
/// audio samples. Calling the `next()` method will
/// return the next [`FrameOut`](enum.FrameOut.html)
/// event from the SAME Receiver or `None` if the
/// available samples have been consumed without any
/// new events.
#[derive(Debug)]
pub struct SourceIter<'rx, I>
where
    I: Iterator<Item = f32>,
{
    source: I,
    receiver: &'rx mut SameReceiver,
}

impl<'rx, 'data, I> Iterator for SourceIter<'rx, I>
where
    I: Iterator<Item = f32>,
{
    type Item = FrameOut;

    fn next(&mut self) -> Option<Self::Item> {
        for sa in &mut self.source {
            match self.receiver.process_high_rate(sa) {
                Some(out) => {
                    info!(
                        "receiver [{:<14}]: {:?}",
                        self.receiver.input_sample_counter(),
                        out
                    );
                    return Some(out);
                }
                _ => continue,
            }
        }

        None
    }
}

fn disabled_equalizer() -> EqualizerBuilder {
    let mut out = EqualizerBuilder::new();
    out.with_filter_order(1, 1);
    out.with_relaxation(0.0);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::io::Write;

    use crate::waveform::{bytes_to_samples, modulate_afsk};

    const TEST_MESSAGE: &str = "ZCZC-ORG-EEE-012345+0000-0001122-NOCALL00-";

    // this method exists to allow us to dump the modulated
    // waveform to a file
    #[allow(dead_code)]
    fn dump_file(out: &[f32], filename: &str) {
        let mut f = std::fs::File::create(filename).expect("Unable to create file");
        for &i in out {
            f.write_all(&(i).to_ne_bytes())
                .expect("Unable to write data");
        }
    }

    fn make_test_message() -> Vec<u8> {
        const PREAMBLE: &[u8] = &[crate::waveform::PREAMBLE; 16];

        let mut message: Vec<u8> = vec![];
        message.extend_from_slice(PREAMBLE);
        message.extend_from_slice(TEST_MESSAGE.as_bytes());
        message
    }

    // Create test burst
    //
    // Returns waveform and number of samples per symbol, at 22.5 kSa/s
    // The returned waveform has all three bursts.
    fn make_test_burst(msg: &[u8]) -> (Vec<f32>, usize) {
        let sample_low = bytes_to_samples(msg, 1);
        let (sample_high, sps) = modulate_afsk(&sample_low, 22050);

        // scale like we're using i16, deliberately not using full arithmetic range
        let burst: Vec<f32> = sample_high.iter().map(|&v| (v * 16384.0f32)).collect();

        let mut out = burst.clone();
        for _i in 0..2 {
            out.extend(std::iter::repeat(0.0f32).take(22050));
            out.extend(burst.iter());
        }
        out.extend(std::iter::repeat(0.0f32).take(22050));

        (out, sps)
    }

    #[test]
    fn test_top_level_receiver() {
        let (afsk, _) = make_test_burst(&make_test_message());

        // uncomment me to dump the output
        //dump_file(&afsk, "output.bin");

        let mut rx = SameReceiverBuilder::new(22050)
            .with_timing_max_deviation(0.01)
            .build();

        println!("{:?}", rx);

        let mut out: Option<crate::Message> = None;
        for evt in rx.iter(afsk.iter().map(|sa| *sa)) {
            if let FrameOut::Ready(Ok(msg)) = evt {
                out = Some(msg);
            }
        }

        assert_eq!(TEST_MESSAGE, out.expect("expected message").as_str());

        // we're waiting for EOM
        assert!(rx.force_eom_at_sample.is_some());

        // force EOM due to timeout
        //   we flush with two seconds of zeros, so putting us 1 second
        //   away from timeout will get the job done during a flush()
        rx.input_sample_counter = rx.force_eom_at_sample.unwrap() - rx.input_rate as u64;
        let msg = rx.flush();
        assert_eq!(Some(Message::EndOfMessage), msg);
    }
}
