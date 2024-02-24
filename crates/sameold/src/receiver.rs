//! Full receiver chain

mod agc;
mod assembler;
mod builder;
mod codesquelch;
mod combiner;
mod dcblock;
mod demod;
mod equalize;
mod filter;
mod framing;
mod output;
mod symsync;
mod timeddata;
mod waveform;

#[cfg(not(test))]
use log::{debug, info, trace, warn};

#[cfg(test)]
use std::{println as debug, println as trace, println as info, println as warn};

use std::convert::From;
use std::iter::{IntoIterator, Iterator};

pub use self::builder::{EqualizerBuilder, SameReceiverBuilder};
pub use self::output::{LinkState, SameEventType, SameReceiverEvent, TransportState};

use crate::Message;

use self::agc::Agc;
use self::assembler::Assembler;
use self::codesquelch::{CodeAndPowerSquelch, SquelchState};
use self::dcblock::DCBlocker;
use self::demod::{Demod, FskDemod};
use self::equalize::Equalizer;
use self::framing::Framer;
use self::symsync::{SymbolEstimate, TimingLoop};

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
/// Once created, use the
/// [`iter_messages()`](SameReceiver::iter_messages)
/// method to obtain decoded messages.
///
/// See [module documentation](index.html) for details.
#[derive(Clone, Debug)]
pub struct SameReceiver {
    dc_block: DCBlocker,
    agc: Agc,
    demod: FskDemod,
    symsync: TimingLoop,
    squelch: CodeAndPowerSquelch,
    equalizer: Equalizer,
    framer: Framer,
    assembler: Assembler,
    timing_bandwidth_unlocked: f32,
    timing_bandwidth_locked: f32,
    input_rate: u32,
    input_sample_counter: u64,
    link_state: LinkState,
    transport_state: TransportState,
    event_queue: std::collections::VecDeque<SameReceiverEvent>,
    ted_sample_clock: u32,
    samples_until_next_ted: f32,
    force_eom_at_sample: Option<u64>,
}

impl SameReceiver {
    /// Decode events and messages from a source of audio
    ///
    /// Bind an iterator which will consume the `input` and
    /// produce SAME [`SameReceiverEvent`] events, which include:
    ///
    /// * notifications about acquired and dropped carrier,
    /// * attempts to frame messages; and
    /// * successful framed messages
    ///
    /// The `input` must be f32 PCM mono audio at
    /// the [`input_rate()`](SameReceiver::input_rate) for this
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
    ///
    /// You can use [`iter_messages()`](SameReceiver::iter_messages)
    /// instead if you are only interested in successful
    /// decodes.
    #[must_use = "iterators are lazy and do nothing unless consumed"]
    pub fn iter_events<'rx, I>(
        &'rx mut self,
        input: I,
    ) -> impl Iterator<Item = SameReceiverEvent> + 'rx
    where
        I: IntoIterator<Item = f32> + 'rx,
    {
        SameReceiverIter {
            receiver: self,
            source: input.into_iter(),
        }
    }

    /// Receive SAME messages from a source of audio
    ///
    /// Bind an iterator which will consume the `input` and
    /// produce SAME [`Message`] events. Only
    /// successfully-decoded messages are reported. Other
    /// events, such as acquisition of signal or decoding
    /// failures, are not reported. If you are interested in
    /// these events, use
    /// [`iter_events()`](SameReceiver::iter_events) instead.
    ///
    /// The `input` must be f32 PCM mono audio at
    /// the [`input_rate()`](SameReceiver::input_rate) for this
    /// receiver. Sound cards commonly output audio samples
    /// in `i16` format. You must perform the conversion to
    /// floating-point yourself, if needed. It is unnecessary
    /// to scale the converted values; our AGC algorithm will
    /// take care of that.
    ///
    /// The iterator will consume as many samples of `input`
    /// that are required to produce the next message. It will
    /// return `None` if the input is exhausted and there
    /// are no new messages.
    #[must_use = "iterators are lazy and do nothing unless consumed"]
    pub fn iter_messages<'rx, I>(&'rx mut self, input: I) -> impl Iterator<Item = Message> + 'rx
    where
        I: IntoIterator<Item = f32> + 'rx,
    {
        self.iter_events(input)
            .filter_map(|evt| evt.into_message_ok())
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
        self.dc_block.reset();
        self.agc.reset();
        self.demod.reset();
        self.symsync.reset();
        self.squelch.reset();
        self.equalizer.reset();
        self.framer.reset();
        self.assembler.reset();
        self.input_sample_counter = 0;
        self.link_state = LinkState::NoCarrier;
        self.transport_state = TransportState::Idle;
        self.event_queue.clear();
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
        let four_seconds_of_zeros = std::iter::repeat(0.0f32)
            .zip(0..self.input_rate * 4)
            .map(|(sa, _)| sa);
        for msg in self.iter_messages(four_seconds_of_zeros) {
            return Some(msg);
        }
        None
    }

    /// Process a sample
    ///
    /// Reads the given iterator of floating-point PCM audio samples.
    /// The audio is demodulated and processed until it is either
    /// exhausted or an event of interest to the modem occurs. If
    /// one does, it is emitted.
    #[inline]
    fn process<I>(&mut self, audio_iter: &mut I) -> Option<SameReceiverEvent>
    where
        I: Iterator<Item = f32>,
    {
        // emit existing events
        while let Some(evt) = self.event_queue.pop_front() {
            return Some(evt);
        }

        // read audio source, process through all layers
        for sample in audio_iter {
            // link-layer processing
            if let Some(link_state) = self.process_linklayer_high_rate(sample) {
                if link_state != self.link_state {
                    // report change
                    self.link_state = link_state.clone();
                    self.event_queue.push_back(SameReceiverEvent::new(
                        self.link_state.clone(),
                        self.input_sample_counter,
                    ));
                }

                // transport-layer processing
                if let Some(transport_state) = self
                    .process_transportlayer(&link_state)
                    .filter(|newstate| newstate != &self.transport_state)
                {
                    self.transport_state = transport_state;
                    self.event_queue.push_back(SameReceiverEvent::new(
                        self.transport_state.clone(),
                        self.input_sample_counter,
                    ));
                }

                if let Some(evt) = self.event_queue.pop_front() {
                    return Some(evt);
                }
            }
        }

        None
    }

    // Transport-layer processing
    //
    // Accepts the new link state. New Bursts are immediately
    // processed through the Assembler. Idle checks are also
    // conducted to:
    //
    // 1. Detect "lingering" SAME messages which exceed the
    //    maximum voice message length
    //
    // 2. Advise the Assembler if no further Bursts are
    //    forthcoming
    //
    // Returns new transport-layer state if one is available.
    #[inline]
    #[must_use]
    fn process_transportlayer(&mut self, link_state: &LinkState) -> Option<TransportState> {
        let transport = match (link_state, self.force_eom_at_sample) {
            (LinkState::Burst(burst_bytes), _) => {
                // Process this burst
                Some(
                    self.assembler
                        .assemble(burst_bytes, self.squelch.symbol_count()),
                )
            }
            (LinkState::NoCarrier, Some(eom_timeout))
                if self.input_sample_counter > eom_timeout =>
            {
                // Timed out waiting for EOM. Manually emit one.
                warn!(
                    "voice message timeout ({} s) exceeded; forcing end-of-message now",
                    Self::MAX_MESSAGE_DURATION_SECS
                );
                Some(TransportState::Message(Ok(Message::EndOfMessage)))
            }
            (LinkState::NoCarrier, _) => {
                // Perform idle processing
                Some(self.assembler.idle(self.squelch.symbol_count()))
            }
            (_, _) => None,
        }?;

        match &transport {
            TransportState::Message(Ok(Message::StartOfMessage(_))) => {
                // set a timer to ensure we will eventually produce an EOM
                // if we miss receipt
                self.force_eom_at_sample = Some(
                    self.input_sample_counter
                        + Self::MAX_MESSAGE_DURATION_SECS * self.input_rate as u64,
                );
            }
            TransportState::Message(Ok(Message::EndOfMessage)) => {
                self.force_eom_at_sample = None;
            }
            _ => {}
        };

        Some(transport)
    }

    // Link-layer processing of one high-rate sample
    //
    // Accepts a floating-point PCM audio sample as `input`
    // and updates the data link layer. Returns the updated
    // link state if a "low-rate" sample was processed or
    // `None` if only high-rate processing was performed.
    #[inline]
    #[must_use]
    fn process_linklayer_high_rate(&mut self, input: f32) -> Option<LinkState> {
        // high-rate processing: dc block, agc, and push onto demodulator's buffer
        let sa = self.agc.input(self.dc_block.filter(input));
        self.demod.push_scalar(sa);
        self.ted_sample_clock += 1;
        self.input_sample_counter = self.input_sample_counter.wrapping_add(1);

        // compute time until we sample for the timing error detector
        // positive → before time, negative → after time
        let clock_remaining_sa = self.samples_until_next_ted - self.ted_sample_clock as f32;
        if clock_remaining_sa <= 0.0f32 || clock_remaining_sa.abs() < 0.5f32 {
            // process low-rate sample and look for state changes
            self.ted_sample_clock = 0;
            let symbol_est = self.process_linklayer_low_rate(clock_remaining_sa)?;
            Some(self.process_linklayer_symbol(&symbol_est))
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
    //
    // Performs demodulation and bit timing error detection.
    // If a bit estimate is ready, returns it. Otherwise, returns
    // `None`.
    #[must_use]
    fn process_linklayer_low_rate(&mut self, clock_remaining_sa: f32) -> Option<SymbolEstimate> {
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

        Some(bit_samples)
    }

    // Process a bit estimate from the bit timing error detector
    //
    // Performs byte synchronization against the SAME preamble
    // (`0xAB`) and framing.
    //
    // Returns updated link state if carrier was detected and
    // the signal was processed all the way through to the framer.
    // Otherwise, returns `None`.
    #[inline]
    #[must_use]
    fn process_linklayer_symbol(&mut self, symbol: &SymbolEstimate) -> LinkState {
        // 3. power and access code correlation squelch
        let (is_resync, squelch_out) = match self.squelch.input(&symbol.data) {
            SquelchState::NoCarrier => {
                // end any frame in progress
                return self.framer.end();
            }
            SquelchState::DroppedCarrier => {
                // end any frame in progress, and reset DSP
                self.end();
                return self.framer.end();
            }
            SquelchState::Reading => {
                // byte not yet ready
                return self.framer.state();
            }
            SquelchState::Ready(true, byte_est) => {
                // when byte sync is achieved, lock down the AGC
                // and bit synchronizer. Put the equalizer
                // in training mode
                debug!(
                    "[{:<14}]: entering tracking mode",
                    self.input_sample_counter()
                );
                self.agc.lock(true);
                self.symsync
                    .set_loop_bandwidth(self.timing_bandwidth_locked);
                self.equalizer
                    .train()
                    .expect("equalizer missing training sequence");
                (true, byte_est)
            }
            SquelchState::Ready(false, byte_est) => {
                // byte ready, no resync
                (false, byte_est)
            }
        };

        // 4. adaptive equalization
        let (byte_est, adaptive_err) = self.equalizer.input(&squelch_out.samples);

        trace!(
            "byte: {:#04x} \"{:?}\", sym pwr: {:0.2}, adapt err: {:0.2}",
            byte_est,
            byte_est as char,
            squelch_out.power,
            adaptive_err
        );

        // 5. framing
        let link_state = self
            .framer
            .input(byte_est, squelch_out.symbol_counter, is_resync);
        match &link_state {
            LinkState::Reading => {
                // prevent sync-like sequences in the message data
                // from changing the sync
                self.squelch.lock(true);
            }
            LinkState::NoCarrier | LinkState::Burst(_) => {
                // reset DSP
                self.end()
            }
            _ => {}
        }

        link_state
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
        debug!(
            "[{:<14}]: returning to acquisition mode",
            self.input_sample_counter()
        );
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
        let sps = waveform::samples_per_symbol(input_rate);
        let (timing_bandwidth_unlocked, timing_bandwidth_locked) = cfg.timing_bandwidth();
        let (power_open, power_close) = cfg.squelch_power();
        let dc_block = DCBlocker::new((cfg.dc_blocker_length() * sps) as usize);
        let agc = Agc::new(
            cfg.agc_bandwidth() * sps / input_rate as f32,
            cfg.agc_gain_limits()[0],
            cfg.agc_gain_limits()[1],
        );
        let demod = FskDemod::new_from_same(cfg.input_rate());
        let symsync = TimingLoop::new(sps, timing_bandwidth_unlocked, cfg.timing_max_deviation());
        let code_squelch = CodeAndPowerSquelch::new(
            waveform::PREAMBLE_SYNC_WORD,
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
            Some(waveform::PREAMBLE_SYNC_WORD),
        );
        let framer = Framer::new(cfg.frame_prefix_max_errors(), cfg.frame_max_invalid());

        let samples_until_next_ted = symsync.samples_per_ted();

        Self {
            dc_block,
            agc,
            demod,
            symsync,
            squelch: code_squelch,
            equalizer,
            framer,
            assembler: Assembler::default(),
            timing_bandwidth_unlocked,
            timing_bandwidth_locked,
            input_rate,
            input_sample_counter: 0,
            link_state: LinkState::NoCarrier,
            transport_state: TransportState::Idle,
            event_queue: std::collections::VecDeque::with_capacity(2),
            ted_sample_clock: 0,
            samples_until_next_ted,
            force_eom_at_sample: None,
        }
    }
}

#[derive(Debug)]
struct SameReceiverIter<'rx, I>
where
    I: Iterator<Item = f32>,
{
    source: I,
    receiver: &'rx mut SameReceiver,
}

impl<'rx, 'data, I> Iterator for SameReceiverIter<'rx, I>
where
    I: Iterator<Item = f32>,
{
    type Item = SameReceiverEvent;

    fn next(&mut self) -> Option<Self::Item> {
        self.receiver.process(&mut self.source).and_then(|evt| {
            info!("{}", &evt);
            Some(evt)
        })
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

    const TEST_MESSAGE: &str = "ZCZC-EAS-DMO-372088-091724-919623-645687-745748-175234-039940-955869-091611-304171-931612-334828-179485-569615-809223-830187-611340-014693-472885-084645-977764-466883-406863-390018-701741-058097-752790-311648-820127-255900-581947+0000-0001122-NOCALL00-";

    // this method exists to allow us to dump the modulated
    // waveform to a file
    #[allow(dead_code)]
    fn dump_file(out: &[f32], filename: &str) {
        let mut f = std::fs::File::create(filename).expect("Unable to create file");
        for &i in out {
            f.write_all(&(i as i16).to_ne_bytes())
                .expect("Unable to write data");
        }
    }

    fn make_test_message(payload: &[u8]) -> Vec<u8> {
        const PREAMBLE: &[u8] = &[waveform::PREAMBLE; 16];

        let mut message: Vec<u8> = vec![];
        message.extend_from_slice(PREAMBLE);
        message.extend_from_slice(payload);
        message
    }

    // Create test burst
    //
    // Returns waveform and number of samples per symbol, at 22.5 kSa/s
    // The returned waveform has `num_bursts` bursts (minimum 1)
    fn make_test_burst(msg: &[u8], num_bursts: usize) -> (Vec<f32>, usize) {
        let sample_low = waveform::bytes_to_samples(msg, 1);
        let (sample_high, sps) = waveform::modulate_afsk(&sample_low, 22050);

        // scale like we're using i16, deliberately not using full arithmetic range
        let burst: Vec<f32> = sample_high.iter().map(|&v| (v * 16384.0f32)).collect();

        let mut out = burst.clone();
        for _i in 1..num_bursts {
            out.extend(std::iter::repeat(0.0f32).take(22050));
            out.extend(burst.iter());
        }
        out.extend(std::iter::repeat(0.0f32).take(2 * 22050));

        (out, sps)
    }

    #[test]
    fn test_iter_events() {
        let (afsk, _) = make_test_burst(&make_test_message(TEST_MESSAGE.as_bytes()), 1);

        let mut rx = SameReceiverBuilder::new(22050)
            .with_timing_max_deviation(0.01)
            .build();

        let mut found = 0usize;
        for (idx, evt) in rx.iter_events(afsk.iter().map(|sa| *sa)).enumerate() {
            match (idx, evt.what()) {
                (0, SameEventType::Link(LinkState::Searching)) => {
                    found += 1;
                }
                (1, SameEventType::Link(LinkState::Reading)) => {
                    found += 1;
                }
                (2, SameEventType::Link(LinkState::Burst(data))) => {
                    assert!(data.starts_with(TEST_MESSAGE.as_bytes()));
                    found += 1;
                }
                (3, SameEventType::Transport(TransportState::Assembling)) => {
                    found += 1;
                }
                (4, SameEventType::Link(LinkState::NoCarrier)) => {
                    found += 1;
                }
                _ => {
                    unreachable!()
                }
            }
        }

        assert_eq!(found, 5);
    }

    #[test]
    fn test_top_level_receiver() {
        let (afsk, _) = make_test_burst(&make_test_message(TEST_MESSAGE.as_bytes()), 3);

        // uncomment me to dump the output
        //dump_file(&afsk, "output.bin");

        let mut rx = SameReceiverBuilder::new(22050)
            .with_timing_max_deviation(0.01)
            .build();

        println!("{:?}", rx);

        let out = rx
            .iter_messages(afsk.iter().map(|sa| *sa))
            .next()
            .expect("expected message");
        assert_eq!(TEST_MESSAGE, out.as_str());

        // we're waiting for EOM
        assert!(rx.force_eom_at_sample.is_some());

        // force EOM due to timeout
        //   we flush with four seconds of zeros, so putting us 3 seconds
        //   away from timeout will get the job done during a flush()
        rx.input_sample_counter = rx.force_eom_at_sample.unwrap() - 3 * rx.input_rate as u64;
        let msg = rx.flush();
        assert_eq!(Some(Message::EndOfMessage), msg);
    }
}
