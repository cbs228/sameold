//! SAME message framing

use std::convert::TryFrom;

use arraydeque::ArrayDeque;
use arrayvec::ArrayVec;

#[cfg(not(test))]
use log::{debug, info, warn};

#[cfg(test)]
use std::println as debug;
#[cfg(test)]
use std::println as info;
#[cfg(test)]
use std::println as warn;

use super::FrameOut;
use crate::message::{Message, MessageDecodeErr};

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
/// At the end of transmission, the `Framer` attempts to
/// parity-correct the last three messages received into
/// one SAME [`Message`](crate::Message). If it can,
/// the message is emitted. If it cannot, a decoding error
/// is reported as a
/// [`MessageDecodeErr`](crate::MessageDecodeErr).
/// Either way, the conclusion of the framing attempt must
/// signal the squelch and equalizer to reset and wait for
/// the next sync.
#[derive(Clone, Debug)]
pub struct Framer {
    // last three messages we have read
    bursts: MessageTriple,

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
            bursts: MessageTriple::new(),
            state: State::Idle,
            symbol_count_last_burst: 0,
            max_prefix_bit_errors,
            max_invalid_bytes,
        }
    }

    /// Reset to zero initial conditions
    pub fn reset(&mut self) {
        self.bursts.clear();
        self.state = State::Idle;
        self.symbol_count_last_burst = 0;
    }

    /// Handle received data byte
    ///
    /// Accepts a single `data` byte from the synchronization
    /// chain and attempts to frame a message. The
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
    /// See [`FrameOut`] for a description of the output.
    pub fn input(&mut self, data: u8, symbol_count: u64, restart: bool) -> FrameOut {
        if restart {
            if symbol_count > self.symbol_count_last_burst + MAX_INTERBURST_GAP_SYMBOLS
                && !self.bursts.is_empty()
            {
                info!("burst: inter-burst time exceeded; discarding saved bursts");
                self.bursts.clear();
            }

            // End the current frame, if we're building one. We will
            // emit an answer if one is Ready.
            let out = self.end(symbol_count);

            debug!("burst: searching: framer restarted");
            self.state = State::PrefixSearch(0, 0);
            let _ = self.input(data, symbol_count, false);
            match out {
                FrameOut::Ready(_) => return out,
                _ => return FrameOut::Searching,
            }
        }

        match self.state {
            // ignore!
            State::Idle => FrameOut::NoCarrier,

            // look for prefix
            State::PrefixSearch(ref mut search_word, ref mut count) => {
                *search_word = (*search_word << 8) | data as u32;
                *count += 1;
                if message_prefix_errors(*search_word) <= self.max_prefix_bit_errors {
                    // found starting prefix!
                    info!("burst: started: after {} bytes", count);

                    let prefix_data = search_word.to_be_bytes();
                    self.state = State::DataRead(
                        Burst::try_from(&prefix_data[0..4]).expect(Self::PANIC_EXPECT_MESSAGE),
                        0,
                    );

                    FrameOut::Reading
                } else if *count > Self::PREFIX_SEARCH_LEN {
                    // give up
                    info!(
                        "burst: abandoned: could not find start after {} bytes",
                        Self::PREFIX_SEARCH_LEN
                    );
                    self.state = State::Idle;
                    FrameOut::NoCarrier
                } else {
                    FrameOut::Searching
                }
            }

            // build the burst
            State::DataRead(ref mut msg, ref mut invalid_byte_count) => {
                *invalid_byte_count += !is_allowed_byte(data) as u32;
                if *invalid_byte_count > self.max_invalid_bytes || msg.try_push(data).is_err() {
                    // we're done!
                    self.end(symbol_count)
                } else {
                    // keep reading
                    FrameOut::Reading
                }
            }
        }
    }

    /// Handle end of frame
    ///
    /// The end of a SAME burst is usually detected automatically.
    /// This method may be used to manually tell the `Framer` that
    /// the current burst has ended. If the Framer was assembling
    /// a burst, the burst is marked as complete and stored. The
    /// Framer will then attempt to assemble the previous three
    /// bursts into a complete message.
    ///
    /// Set `symbol_count` to the lifetime count of symbols
    /// received by the
    /// [`CodeAndPowerSquelch`](super::codesquelch::CodeAndPowerSquelch).
    ///
    /// See [`FrameOut`] for a description of the output.
    pub fn end(&mut self, symbol_count: u64) -> FrameOut {
        // if we're reading a frame, that frame is done
        match self.state {
            State::DataRead(ref mut msg, _) => {
                info!("burst: ended: \"{}\"", String::from_utf8_lossy(msg));

                // add to our list of completed bursts
                self.symbol_count_last_burst = symbol_count;
                fill_remaining(msg, 0);
                self.bursts
                    .push_back(msg.take().into_inner().expect(Self::PANIC_EXPECT_FULL));
                self.state = State::Idle;

                // can we build a message out of the last three bursts?
                if self.bursts.is_full() {
                    // got three bursts; try to frame them
                    FrameOut::Ready(try_recover_message(&mut self.bursts))
                } else if message_prefix_is_eom(self.bursts.back().unwrap()) {
                    // fast EOM: we report every NNNN burst received immediately
                    FrameOut::Ready(Ok(Message::EndOfMessage))
                } else {
                    FrameOut::NoCarrier
                }
            }
            _ => {
                self.state = State::Idle;
                FrameOut::NoCarrier
            }
        }
    }

    // once started, search a total of 21 bytes for
    // a valid data start prefix (16 bytes preamble + 4 bytes prefix + 1 byte margin)
    const PREFIX_SEARCH_LEN: u32 = 21;

    const PANIC_EXPECT_MESSAGE: &'static str = "expected populated message during DataRead";
    const PANIC_EXPECT_FULL: &'static str = "expected fully-populated ArrayVec";
}

// Maximum SAME/EAS frame length, in bytes
//
// If a frame is read which exceeds this length, it will be
// truncated.
const MAX_MESSAGE_LENGTH: usize = 268;

// Maximum time between bursts, in symbols
//
// Maximum number of symbols between the end of one burst and
// the start of the next.
const MAX_INTERBURST_GAP_SYMBOLS: u64 = (1.5 * super::waveform::BAUD_HZ) as u64;

// SAME data burst
//
// Contains bytes from one of three (hopefully identical)
// SAME data bursts
type Burst = ArrayVec<u8, MAX_MESSAGE_LENGTH>;

// A trio of SAME/EAS bursts
//
// Each burst is always fully-populated, with empty elements
// filled with zeroes.
type MessageTriple = ArrayDeque<[[u8; MAX_MESSAGE_LENGTH]; 3], arraydeque::Wrapping>;

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
    DataRead(Burst, u32),
}

// Try to build a complete valid message from three bursts
//
// If a message is emitted, the bursts buffer is cleared.
fn try_recover_message(bursts: &mut MessageTriple) -> Result<Message, MessageDecodeErr> {
    let mut out = Burst::new();
    let mut errs = Burst::new();
    if let Some(msg) = correct_errors(bursts.iter(), &mut out, &mut errs) {
        // if we get a valid message, clear the bursts buffer to
        // prevent later messages from being conflated with
        // this one
        bursts.clear();
        match Message::try_from((msg.to_owned(), errs.as_slice())) {
            Ok(out) => {
                info!("message ({} errors): \"{}\"", out.parity_error_count(), out);
                Ok(out)
            }
            Err(e) => {
                warn!("decode failure ({}): \"{}\"", e, msg);
                Err(e)
            }
        }
    } else {
        // the above will fail if the message contains non utf-8
        Err(MessageDecodeErr::NotAscii)
    }
}

// Combine three SAME/EAS bursts into one message estimate
//
// If this method returns `Some`, the message consists of
// only valid SAME/EAS characters and converts correctly
// to a string, but no other guarantees are made. The
// message is written to `out`, and the bit error count
// per byte is written ot `err_counts`.
fn correct_errors<'b, 'o, B, S>(
    mut burst_iter: B,
    out: &'o mut Burst,
    err_counts: &'_ mut Burst,
) -> Option<&'o str>
where
    B: ExactSizeIterator<Item = &'b S>,
    S: AsRef<[u8]> + 'b,
{
    err_counts.clear();
    out.clear();

    // iterate over bytes in all input buffers,
    for ((b0, b1), b2) in burst_iter
        .next()?
        .as_ref()
        .iter()
        .zip(burst_iter.next()?.as_ref().iter())
        .zip(burst_iter.next()?.as_ref().iter())
    {
        let (byte_out, errs) = bit_vote_parity(*b0, *b1, *b2);

        // the upper bit should always be zero, mask it
        // if the character is permitted after correction,
        // accept it… but mark it as an error too
        let byte_out_masked = byte_out & 0x7f;

        // is the byte valid?
        if !is_allowed_byte(byte_out_masked) {
            break;
        }

        if out.try_push(byte_out_masked).is_err() {
            break;
        }

        if err_counts
            .try_push(errs as u8 + (byte_out != byte_out_masked) as u8)
            .is_err()
        {
            break;
        }
    }

    // this *should* convert to str okay given what
    // bytes we allow, but add an out for us in case
    // it does not
    match std::str::from_utf8(out.as_slice()) {
        Ok(s) => Some(s),
        Err(_e) => None,
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

// True if the burst starts with the EOM sequence
fn message_prefix_is_eom(inp: &[u8]) -> bool {
    if inp.len() < 2 {
        return false;
    }

    &inp[0..2] == ['N' as u8, 'N' as u8]
}

// Two-of-three bit voting
//
// Assume b0…b2 are multiple repetitions of the
// same byte. For each bit, pick the most popular
// state (set or unset). Return the corrected byte
// and the total number of bits with disagreements.
//
// This is a really dumb error-correction scheme.
// You shouldn't use it. Seriously. Don't!
#[inline]
fn bit_vote_parity(b0: u8, b1: u8, b2: u8) -> (u8, u32) {
    // inverse-XOR together. Ones indicate agreement.
    // zeroes indicate disputes.
    let pair0 = !(b0 ^ b1);
    let pair1 = !(b1 ^ b2);
    let pair2 = !(b0 ^ b2);

    // now pair0 is 1 where we want to take from the
    // first pair, pair1 is 1 where we want to
    // take from the second pair, and pair2 is 1
    // where we want to take from the third pair
    (
        (b0 & pair0) | (b2 & pair1) | (b2 & pair2),
        (pair0 & pair1 & pair2).count_zeros(),
    )
}

// Is the given byte an allowed SAME/EAS character?
//
// Allowed characters include the following ASCII:
// - Uppercase letters
// - Lowercase letters
// - Numbers
// - Minus sign (`-`)
// - Plus sign (`+`)
// - Question mark (`?`)
// - Open parentheses (`(`)
// - Close parentheses (`)`)
// - Open brackets (`[`)
// - Close brackets (`]`)
// - Period (`.`)
// - Underscore (`_`)
// - Comma (`,`)
// - Slash (`/`)
// - Space (` `)(might be encountered in callsign field)
//
// The preamble byte `0xAB` is not allowed after the
// data transmission begins, and it is not marked
// as allowed here.
#[inline]
fn is_allowed_byte(c: u8) -> bool {
    const MINUS: u8 = '-' as u8;
    const PLUS: u8 = '+' as u8;
    const QUESTION_MARK: u8 = '?' as u8;
    const OPEN_PARENTHESES: u8 = '(' as u8;
    const CLOSE_PARENTHESES: u8 = ')' as u8;
    const OPEN_BRACKETS: u8 = '[' as u8;
    const CLOSE_BRACKETS: u8 = ']' as u8;
    const PERIOD: u8 = '.' as u8;
    const UNDERSCORE: u8 = '_' as u8;
    const COMMA: u8 = ',' as u8;
    const SLASH: u8 = '/' as u8;
    const SPACE: u8 = ' ' as u8;
    const NUMBERS: [u8; 2] = ['0' as u8, '9' as u8];
    const UPPER_ALPHA: [u8; 2] = ['A' as u8, 'Z' as u8];
    const LOWER_ALPHA: [u8; 2] = ['a' as u8, 'z' as u8];

    c == MINUS
        || (c >= NUMBERS[0] && c <= NUMBERS[1])
        || (c >= UPPER_ALPHA[0] && c <= UPPER_ALPHA[1])
        || (c >= LOWER_ALPHA[0] && c <= LOWER_ALPHA[1])
        || c == SLASH
        || c == QUESTION_MARK
        || c == OPEN_PARENTHESES
        || c == CLOSE_PARENTHESES
        || c == OPEN_BRACKETS
        || c == CLOSE_BRACKETS
        || c == PERIOD
        || c == UNDERSCORE
        || c == COMMA
        || c == PLUS
        || c == SPACE
}

// fill remaining elements of `arr` with `val`
fn fill_remaining<T, const CAP: usize>(arr: &mut ArrayVec<T, CAP>, val: T)
where
    T: Copy,
{
    for _i in 0..arr.remaining_capacity() {
        arr.push(val)
    }
}

#[cfg(test)]
mod tests {
    use super::super::waveform;
    use super::*;

    use std::convert::AsRef;

    #[test]
    fn test_bit_vote() {
        // these are easy
        assert_eq!(bit_vote_parity(0xab, 0xab, 0xab), (0xab, 0));
        assert_eq!(bit_vote_parity(0xff, 0xff, 0xff), (0xff, 0));
        assert_eq!(bit_vote_parity(0x00, 0x00, 0x00), (0x00, 0));

        // mixed bits
        assert_eq!(bit_vote_parity(0xaa, 0xab, 0xab), (0xab, 1));
        assert_eq!(bit_vote_parity(0xa0, 0xa0, 0xaf), (0xa0, 4));
        assert_eq!(bit_vote_parity(0x0f, 0xf0, 0xff), (0xff, 8));
        assert_eq!(bit_vote_parity(0x00, 0xf0, 0xff), (0xf0, 8));
        assert_eq!(bit_vote_parity(0xaa, 0x55, 0xff), (0xff, 8));
        assert_eq!(bit_vote_parity(0xaa, 0x55, 0xa5), (0xa5, 8));
    }

    #[test]
    fn test_correct_errors() {
        const ONE: &[u8] = &[0x5a, 0x43, 0x5a, 0x43, 0xab, 0x00]; // ZCZC plus garbage
        const TWO: &[u8] = &[0x50, 0x43, 0x5a, 0x43, 0x00, 0x01, 0x00];
        const THREE: &[u8] = &[0x5a, 0x42, 0x0a, 0x43, 0x0a, 0xff];
        const TRIPLE: &[&[u8]] = &[ONE, TWO, THREE];

        const EXPECT_ERRORS: &[u8] = &[2, 1, 2, 0];

        let mut outbuf = Burst::new();
        let mut errs = Burst::new();
        let outstr = correct_errors(TRIPLE.iter(), &mut outbuf, &mut errs).expect("decode err");
        assert!(outstr.starts_with("ZCZC"));
        assert_eq!(EXPECT_ERRORS, &errs[0..4]);
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
    fn test_message_prefix_is_eom() {
        assert!(!message_prefix_is_eom(&[]));
        assert!(!message_prefix_is_eom("NZK".as_bytes()));
        assert!(message_prefix_is_eom("NNNNzzzz!".as_bytes()));
        assert!(message_prefix_is_eom(&['N' as u8, 'N' as u8, 5, 11, 10, 0]));
    }

    #[test]
    fn test_arrayvec() {
        let mut m = Burst::new();
        m.push(1);
        fill_remaining(&mut m, 0);
        assert!(m.is_full());
        assert_eq!(m[0], 1);
        assert_eq!(m[1], 0);
    }

    #[test]
    fn test_framer_prefix() {
        const DATA: &[u8] = &['Z' as u8, 'C' as u8, 'Z' as u8, 'C' as u8];

        let mut framer = Framer::new(1, 10);

        // we give up if too many preamble bytes are received
        let mut gave_up = false;
        for i in 0..32 {
            match framer.input(waveform::PREAMBLE, 0, i == 0) {
                FrameOut::NoCarrier => {
                    assert!(i >= Framer::PREFIX_SEARCH_LEN);
                    gave_up = true;
                }
                FrameOut::Searching => {}
                _ => unreachable!(),
            }
        }
        assert!(gave_up);

        // we enter Reading mode after we get the prefix
        framer.input(waveform::PREAMBLE, 0, true);
        framer.input(waveform::PREAMBLE, 0, true);
        let mut last = FrameOut::NoCarrier;
        for d in DATA {
            last = framer.input(*d, 0, false);
            assert!(last.is_active());
        }
        assert_eq!(last, FrameOut::Reading);
        match &framer.state {
            State::DataRead(msg, 0) => {
                assert_eq!(msg.len(), 4);
                assert_eq!(&msg[0..4], DATA);
            }
            _ => unreachable!(),
        }

        // force it to end
        assert_eq!(FrameOut::NoCarrier, framer.end(0));
        assert_eq!(framer.bursts.len(), 1);
    }

    #[test]
    fn test_framer_message() {
        const MAX_LEN_THREE: usize = 3 * MAX_MESSAGE_LENGTH;

        // if we provide a *bunch* of "N", this decodes as end of message
        let mut framer = Framer::new(1, 10);

        for i in 0..MAX_LEN_THREE {
            let out = framer.input('N' as u8, 0, i == 0);
            if i > 0 && i % MAX_MESSAGE_LENGTH == 0 {
                // expect carrier drop because buffer is full
                // but we declare EOM since we got enough N.
                assert_eq!(out, FrameOut::Ready(Ok(Message::EndOfMessage)));

                // restart it
                framer.input('N' as u8, 0, true);
                continue;
            }
            match i {
                MAX_LEN_THREE => assert_eq!(out, FrameOut::Ready(Ok(Message::EndOfMessage))),
                _ => assert!(out.is_active()),
            }
        }
    }

    #[test]
    fn test_framer_message_assemble() {
        // try a realistic message
        const MESSAGE: &str = "gArbAZgEZCZC-ORG-EEE-012345-567890+0000-0001122-NOCALL00-GARBAGE";
        const PERMIT_INVALID: u32 = 10;

        let mut found = false;
        let mut framer = Framer::new(2, PERMIT_INVALID);
        for i in 0..3 {
            // preamble turns the framer on
            framer.input(waveform::PREAMBLE, 0, true);

            // we read the data
            for c in MESSAGE.as_bytes() {
                assert!(framer.input(*c, 0, false).is_active());
            }

            // and provide some preambles until we error out
            for j in 0..PERMIT_INVALID + 1 {
                let out = framer.input(waveform::PREAMBLE, 0, false);
                if j >= PERMIT_INVALID {
                    if i < 2 {
                        // enough invalid chars to make us drop
                        assert_eq!(out, FrameOut::NoCarrier);
                    } else {
                        // enough invalid chars to trigger framing
                        if let FrameOut::Ready(Ok(m)) = out {
                            assert_eq!(
                                "ZCZC-ORG-EEE-012345-567890+0000-0001122-NOCALL00-",
                                m.as_ref()
                            );
                            found = true;
                        } else {
                            unreachable!();
                        }
                    }
                } else {
                    // still reading
                    assert!(out.is_active());
                }
            }
        }

        assert_eq!(framer.bursts.len(), 0);
        assert!(found);
    }

    #[test]
    fn test_framer_message_timeout() {
        // If we exceed the maximum inter-burst time, discard the buffer
        // and start over
        const MESSAGE: &str = "ZCZC-ORG-EEE-012345-567890+0000-0001122-NOCALL00-";

        let mut framer = Framer::new(2, 10);
        for _i in 0..2 {
            // preamble turns the framer on
            framer.input(waveform::PREAMBLE, 0, true);

            // we read the data
            for c in MESSAGE.as_bytes() {
                assert!(framer.input(*c, 0, false).is_active());
            }

            // manually end
            let _ = framer.end(0);
        }

        assert_eq!(2, framer.bursts.len());

        // if it has been too long, we clear the old buffers
        framer.input(waveform::PREAMBLE, MAX_INTERBURST_GAP_SYMBOLS + 1, true);
        assert_eq!(0, framer.bursts.len());
    }

    #[test]
    fn test_framer_fast_eom() {
        // EOM with two bit errors
        const MESSAGE: &str = "NNLLZZ";

        let mut framer = Framer::new(2, 10);

        framer.input(waveform::PREAMBLE, 0, true);

        for c in MESSAGE.as_bytes() {
            assert!(framer.input(*c, 0, false).is_active());
        }

        // manually end; get EOM
        assert_eq!(FrameOut::Ready(Ok(Message::EndOfMessage)), framer.end(0));
    }
}
