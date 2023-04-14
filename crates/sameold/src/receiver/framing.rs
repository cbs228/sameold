//! SAME message framing

#[cfg(not(test))]
use log::{debug, info};

#[cfg(test)]
use std::println as debug;
#[cfg(test)]
use std::println as info;

use super::assembler;
use super::combiner;
use super::LinkState;

/// SAME/EAS message framer
///
/// The `Framer` does nothing until the
/// [`CodeAndPowerSquelch`](super::codesquelch::CodeAndPowerSquelch)
/// or some other process invokes the [`input()`](#method.input)
/// method.
/// with the `restart` flag set. This indicates that the
/// receiver has detected the preamble and has synchronized
/// to byte boundaries. You may use the `restart` flag at
/// any time to discard the state and start over.
///
/// Once started, the [`input()`](#method.input) will accept
/// one byte at a time and search for the beginning of the
/// message. SAME messages begin with either "`ZCZC`" or
/// "`NNNN`." We will then read until a certain number of
/// invalid bytes have been read. Invalid bytes are, sadly,
/// one of the more practical ways of detecting the end of
/// the frame. If you have some other knowledge of where the
/// end of the frame is, you may manually call the
/// [`end()`](#method.end) method.
///
/// At the end of transmission, the `Framer` issues a
/// [`LinkState::Burst`] with an estimate of the received bytes.
#[derive(Clone, Debug)]
pub struct Framer {
    // framer state
    state: State,

    // lifetime symbol count at last burst end()
    symbol_count_last_burst: u64,

    // how closely must the received data match the message prefix sequences?
    max_prefix_bit_errors: u32,

    // exit data read after this many uncorrected byte errors
    max_invalid_bytes: u32,
}

impl Framer {
    /// New Framer
    ///
    /// The Framer will look for "beginning of data" prefixes
    /// (like `ZCZC` and `NNNN`) with `max_prefix_bit_errors`
    /// bit errors. Set this to a small value, like `2`.
    ///
    /// The Framer will auto-detect the end of a burst when more
    /// than `max_invalid_bytes` invalid bytes have been received.
    ///
    /// * If this value is zero, the burst will not permit any
    ///   invalid bytes… but then the parity correction mechanism
    ///   loses the chance to correct any errors.
    ///
    /// * If the value is set too high, the Framer might not drop
    ///   carrier in between bursts. This can be a problem if the
    ///   synchronization mechanism (i.e., `CodeAndPowerSquelch`)
    ///   won't re-sync during a frame.
    pub fn new(max_prefix_bit_errors: u32, max_invalid_bytes: u32) -> Self {
        Self {
            state: State::Idle,
            symbol_count_last_burst: 0,
            max_prefix_bit_errors,
            max_invalid_bytes,
        }
    }

    /// Reset to zero initial conditions
    pub fn reset(&mut self) {
        self.state = State::Idle;
        self.symbol_count_last_burst = 0;
    }

    /// Handle received data byte
    ///
    /// Accepts a single `data` byte from the synchronization
    /// chain and attempts to frame a burst. The
    /// `symbol_count` should be set to the lifetime count of
    /// symbols received by the
    /// [`CodeAndPowerSquelch`](super::codesquelch::CodeAndPowerSquelch).
    ///
    /// Whenever the preamble sequence is detected and has been
    /// synchronized to, the caller must set the `restart` flag
    /// to `true`. This indicates that a new SAME data burst
    /// has begun.
    ///
    /// Once started with `restart,` the Framer will read data
    /// until any of the following:
    ///
    /// 1. The maximum data burst length is exceeded
    /// 2. The caller invokes [`end()`](#method.end) to terminate
    ///    the burst
    /// 3. More than `max_invalid_bytes` are received, which is
    ///    an indication that the carrier has been dropped.
    ///
    /// See [`LinkState`] for a description of the output.
    pub fn input(&mut self, data: u8, symbol_count: u64, restart: bool) -> LinkState {
        if restart {
            // End the current frame, if we're building one. We will
            // emit an answer if one is Ready.
            let out = self.end();

            debug!("burst: searching: framer restarted");
            self.state = State::PrefixSearch(0, 0);
            let _ = self.input(data, symbol_count, false);
            match out {
                LinkState::Burst(_) => return out,
                _ => return LinkState::Searching,
            }
        }

        match self.state {
            // ignore!
            State::Idle => LinkState::NoCarrier,

            // look for prefix
            State::PrefixSearch(ref mut search_word, ref mut count) => {
                *search_word = (*search_word << 8) | data as u32;
                *count += 1;
                if message_prefix_errors(*search_word) <= self.max_prefix_bit_errors {
                    // found starting prefix!
                    info!("burst: started: after {} bytes", count);

                    let prefix_data = search_word.to_be_bytes();
                    let mut data = Vec::with_capacity(assembler::MAX_MESSAGE_LENGTH);
                    data.extend_from_slice(&prefix_data[0..4]);
                    self.state = State::DataRead(data, 0);
                } else if *count > Self::PREFIX_SEARCH_LEN {
                    // give up
                    info!(
                        "burst: abandoned: could not find start after {} bytes",
                        Self::PREFIX_SEARCH_LEN
                    );
                    self.state = State::Idle;
                }
                self.state()
            }

            // build the burst
            State::DataRead(ref mut msg, ref mut invalid_byte_count) => {
                *invalid_byte_count += !combiner::is_allowed_byte(data) as u32;
                if *invalid_byte_count > self.max_invalid_bytes {
                    // we're done!
                    self.end()
                } else {
                    // keep reading
                    msg.push(data);
                    self.state()
                }
            }
        }
    }

    /// Handle end of frame
    ///
    /// The end of a SAME burst is usually detected automatically.
    /// This method may be used to manually tell the `Framer` that
    /// the current burst has ended. If the Framer was assembling
    /// a burst, the burst is marked as complete and emitted.
    ///
    /// See [`LinkState`] for a description of the output.
    pub fn end(&mut self) -> LinkState {
        // if we're reading a frame, that frame is done
        match std::mem::take(&mut self.state) {
            State::DataRead(msg, _) => {
                self.state = State::Idle;
                LinkState::Burst(msg)
            }
            _ => {
                self.state = State::Idle;
                LinkState::NoCarrier
            }
        }
    }

    /// Return the state as of the last input
    ///
    /// Reports the state of the framer without providing any new input
    pub fn state(&self) -> LinkState {
        match &self.state {
            State::Idle => LinkState::NoCarrier,
            State::PrefixSearch(..) => LinkState::Searching,
            State::DataRead(..) => LinkState::Reading,
        }
    }

    // once started, search a total of 21 bytes for
    // a valid data start prefix (16 bytes preamble + 4 bytes prefix + 1 byte margin)
    const PREFIX_SEARCH_LEN: u32 = 21;
}

// Framer state
#[derive(Clone, Debug, PartialEq, Eq)]
enum State {
    // Do nothing. Eat the input.
    Idle,

    // Search for prefix sequence ZCZC or NNNN
    //
    // Payload is last four bytes (as packed u32) and a
    // timeout byte counter
    PrefixSearch(u32, u32),

    // Read message data bytes
    //
    // Payload is the message being read and the invalid
    // byte count. Once the invalid byte count reaches a
    // critical value, the data read ends.
    DataRead(Vec<u8>, u32),
}

impl Default for State {
    fn default() -> Self {
        Self::Idle
    }
}

// Calculate bit errors for message prefix
//
// Search the word-packed bytes of `inp` for the message prefixes
// "ZCZC" and "NNNN." Returns minimum number of bit errors to
// either prefix.
fn message_prefix_errors(inp: u32) -> u32 {
    const PREFIX_BYTES_START: u32 =
        u32::from_be_bytes(['Z' as u8, 'C' as u8, 'Z' as u8, 'C' as u8]);
    const PREFIX_BYTES_END: u32 = u32::from_be_bytes(['N' as u8, 'N' as u8, 'N' as u8, 'N' as u8]);

    let err_start = (inp ^ PREFIX_BYTES_START).count_ones();
    let err_end = (inp ^ PREFIX_BYTES_END).count_ones();
    u32::min(err_start, err_end)
}

#[cfg(test)]
mod tests {
    use super::super::waveform;
    use super::*;

    fn is_link_active(lx: &LinkState) -> bool {
        match lx {
            LinkState::Searching => true,
            LinkState::Reading => true,
            _ => false,
        }
    }

    #[test]
    fn test_message_prefix_errors() {
        const START: [u8; 4] = ['Z' as u8, 'C' as u8, 'Z' as u8, 'C' as u8];
        const END: [u8; 4] = ['N' as u8, 'N' as u8, 'N' as u8, 'N' as u8];
        const PREAMBLE: [u8; 4] = [171, 171, 171, 171];
        const ZCZE: [u8; 4] = ['Z' as u8, 'C' as u8, 'Z' as u8, 'E' as u8];

        assert_eq!(0, message_prefix_errors(u32::from_be_bytes(START)));
        assert_eq!(0, message_prefix_errors(u32::from_be_bytes(END)));
        assert_eq!(18, message_prefix_errors(u32::from_be_bytes(PREAMBLE)));
        assert_eq!(2, message_prefix_errors(u32::from_be_bytes(ZCZE)));
    }

    #[test]
    fn test_framer_prefix() {
        const DATA: &[u8] = &['Z' as u8, 'C' as u8, 'Z' as u8, 'C' as u8];

        let mut framer = Framer::new(1, 10);

        // we give up if too many preamble bytes are received
        let mut gave_up = false;
        for i in 0..32 {
            match framer.input(waveform::PREAMBLE, 0, i == 0) {
                LinkState::NoCarrier => {
                    assert!(i >= Framer::PREFIX_SEARCH_LEN);
                    gave_up = true;
                }
                LinkState::Searching => {}
                _ => unreachable!(),
            }
        }
        assert!(gave_up);

        // we enter Reading mode after we get the prefix
        framer.input(waveform::PREAMBLE, 0, true);
        framer.input(waveform::PREAMBLE, 0, true);
        let mut last = LinkState::NoCarrier;
        for d in DATA {
            last = framer.input(*d, 0, false);
            assert!(is_link_active(&last));
        }
        assert_eq!(last, LinkState::Reading);
        match &framer.state {
            State::DataRead(msg, 0) => {
                assert_eq!(msg.len(), 4);
                assert_eq!(&msg[0..4], DATA);
            }
            _ => unreachable!(),
        }

        // force it to end
        assert_eq!(LinkState::Burst("ZCZC".as_bytes().to_owned()), framer.end());
        assert_eq!(LinkState::NoCarrier, framer.end());
    }

    #[test]
    fn test_framer_burst_process() {
        // try a realistic message
        const MESSAGE: &str = "gArbAZgEZCZC-ORG-EEE-012345-567890+0000-0001122-NOCALL00-GARBAGE";
        const PERMIT_INVALID: u32 = 10;

        let mut found = false;
        let mut framer = Framer::new(2, PERMIT_INVALID);

        // preamble turns the framer on
        framer.input(waveform::PREAMBLE, 0, true);

        // we read the data
        for c in MESSAGE.as_bytes() {
            assert!(is_link_active(&framer.input(*c, 0, false)));
        }

        // and provide some preambles until we error out
        for j in 0..PERMIT_INVALID + 1 {
            let out = framer.input(waveform::PREAMBLE, 0, false);
            if j >= PERMIT_INVALID {
                if let LinkState::Burst(m) = out {
                    assert!(&m.starts_with(
                        "ZCZC-ORG-EEE-012345-567890+0000-0001122-NOCALL00-".as_bytes()
                    ));
                    found = true;
                } else {
                    unreachable!();
                }
            } else {
                // still reading
                assert!(is_link_active(&out));
            }
        }

        assert!(found);
    }
}
