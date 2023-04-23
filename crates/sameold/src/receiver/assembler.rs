//! Assembles SAME bursts into a Message
//!
//! SAME bursts have the following timing:
//!
//! * There is no gap between program audio and the start of a SAME message
//! * Successive bursts are separated by one second of silence (±5%)
//! * There is no dead time at the end of message
//!
//! While many SAME messages include a Warning Alarm Tone (WAT) and
//! a voice message, there is no requirement for either. SAME messages
//! may be terminated one second after they begin with an EOM.
//!
//! These timings make it difficult to design a decoder that is robust
//! against missing bursts—but the [`Assembler`] makes an effort. We
//! follow these rules:
//!
//! 1. There is always room to improve a previous message estimate.
//!    Messages will be held until the inter-burst time (~1.311 seconds)
//!    expires. After that, if a valid message is present, it is
//!    emitted. See `sameold::assembler::MAX_INTERBURST_SYMBOLS`.
//!
//!    * We must wait until the [framer](super::framing) has been idle
//!      for awhile before we emit a message.
//!
//!    * The delay is not troublesome since most SAME messages include
//!      up to ten seconds of Warning Alarm Tone, which is not
//!      information-bearing.
//!
//! 2. An exception to Rule (1) is that Fast EOMs are permitted to
//!    issue immediately.
//!
//! 3. We keep previous ("historical") bursts around until they cannot
//!    possibly combine with a new burst to form a message.
//!
//!    * This means keeping at least two previous bursts…. which each
//!      permitted to be *maximum-length* messages.
//!      See `crate::assembler::MAX_HISTORY_DURATION`, which is about
//!      10.86 seconds.
//!
//!    * With missing bursts and very short messages, we might
//!      accidentally mix the current SAME message with the next
//!      one. This means we can't clear the history when we emit
//!      a message.
//!
//! 4. As a consequence of Rule (3), we must detect *duplicate* messages
//!    and suppress them.
//!
//!
use std::iter::FromIterator;

#[cfg(not(test))]
use log::debug;

#[cfg(test)]
use std::println as debug;

use crate::message::{Message, MessageResult};

use super::combiner;
use super::output::TransportState;
use super::timeddata::TimedData;
use super::waveform;

/// Maximum SAME/EAS frame length, in bytes
///
/// If a frame is read which exceeds this length, it will be
/// truncated. The maximum message length includes all
/// 16 bytes for the preamble—even though these are not
/// generally recorded in a Burst.
pub(crate) const MAX_MESSAGE_LENGTH: usize = 268;

/// Maximum time between bursts, in symbols
///
/// The
/// [SAME specification](https://www.nws.noaa.gov/directives/sym/pd01017012curr.pdf)
/// reads:
///
/// > The preamble and header code are transmitted three (3) times, with a
/// > one-second pause (±5%) between each coded message burst prior to the
/// > broadcast of the actual voice message.
///
/// We additionally permit up to 16 bytes of delay to permit the
/// preamble [squelch](super::codesquelch) to detect the preamble
/// and acquire byte sync.
pub(crate) const MAX_INTERBURST_SYMBOLS: u64 = ((1.05 * waveform::BAUD_HZ) + 17.0 * 8.0) as u64;

/// Maximum SAME burst history
///
/// The maximum amount of history, in SAME symbols, that must be
/// retained in order to fit three maximum-length SAME bursts in
/// memory.
pub(crate) const MAX_HISTORY_DURATION: u64 =
    2 * (MAX_INTERBURST_SYMBOLS + 8 * MAX_MESSAGE_LENGTH as u64);

/// SAME data burst
///
/// Contains bytes from one of three (hopefully identical)
/// SAME data bursts
pub(crate) type Burst = arrayvec::ArrayVec<u8, MAX_MESSAGE_LENGTH>;

/// The Assembler collects Bursts into Messages
///
/// A SAME message is nominally composed of three identical repetitions
/// of an ASCII string. This object collects bursts together into a
/// single [`Message`](crate::message::Message).
///
/// Higher-level logic is responsible for providing a *symbol count*:
/// i.e., a monotonic counter which advances at the SAME symbol rate
/// of [`waveform::BAUD_HZ`]. This is needed to permit bursts to
/// timeout if not all are received.
#[derive(Clone, Debug)]
pub struct Assembler {
    history: BurstHistory,
    state: PendingResult,
    previous: Option<PreviousMessage>,
}

impl Assembler {
    /// New Assembler
    pub fn new() -> Self {
        Self {
            history: BurstHistory::with_capacity(3),
            state: PendingResult::default(),
            previous: Option::default(),
        }
    }

    /// Reset to zero initial conditions
    pub fn reset(&mut self) {
        self.history.clear();
        self.state = PendingResult::default();
        self.previous = Option::default();
    }

    /// Attempt to build the given burst into a message
    ///
    /// Accepts a SAME data burst, adds it to the history, and
    /// attempts to form the history into a message. If a message
    /// or error is ready to report, returns
    /// [`TransportState::Message`].
    ///
    /// Most messages will not assemble immediately. Instead, the
    /// assembler waits optimistically for a new bursts which will
    /// potentially improve the decoding. This permits the assembler
    /// to report SAME messages which are incompletely
    /// received—i.e., when only two of three bursts are received.
    /// The receiver must poll the [`Assembler::idle()`] method when
    /// it is idle to retrieve these.
    ///
    /// The `symbol_count` must be the symbol count as of the
    /// **end** of the given `burst`. The symbol count is a monotonic
    /// timer that advances with every output of the
    /// [symbol synchronizer](super::symsync).
    pub fn assemble<B>(&mut self, burst: B, symbol_count: u64) -> TransportState
    where
        B: AsRef<[u8]>,
    {
        // if no data, just do idle processing
        let burst = burst.as_ref();
        if burst.is_empty() {
            return self.idle(symbol_count);
        }

        // append to history
        prune_history(&mut self.history, symbol_count);
        prune_previous(&mut self.previous, symbol_count);
        self.history.push_back(TimedData::with_deadline(
            Burst::from_iter(
                burst[0..usize::min(burst.len(), MAX_MESSAGE_LENGTH)]
                    .iter()
                    .copied(),
            ),
            symbol_count + MAX_HISTORY_DURATION,
        ));

        // parse message from history, deduplicate it
        if let Some(msg) = self.deduplicate(combiner::combine(self.bursts())) {
            // if the burst history forms a new message, store it.
            // this potentially replaces a previous stored message
            self.state.accept(msg, symbol_count);
        }

        self.idle(symbol_count)
    }

    /// Check for a pending message if the framer is idle
    ///
    /// The `Assembler` optimistically retains messages for a
    /// period of time to see if a better decode is forthcoming.
    /// The caller must invoke this method whenever the framer is
    /// idle and is *not* reading a message at the given
    /// `symbol_count` time.
    ///
    /// If the timeout on any pending message (or error) has
    /// elapsed, it is immediately emitted as an
    /// [`TransportState::Message`].
    ///
    /// All emitted messages emitted are stored in a de-duplication
    /// buffer, with a "reasonable" timeout value, to prevent
    /// duplicate messages from being emitted.
    ///
    /// The symbol count is a monotonic timer that advances with
    /// every output of the
    /// [symbol synchronizer](super::symsync).
    pub fn idle(&mut self, symbol_count: u64) -> TransportState {
        prune_history(&mut self.history, symbol_count);

        match self.state.poll(symbol_count) {
            Some(Ok(msg)) => {
                debug!(
                    "assembler: ready ({} voting, {} errors): \"{}\"",
                    msg.voting_byte_count(),
                    msg.parity_error_count(),
                    msg
                );
                self.previous = Some(TimedData::with_deadline(
                    msg.clone(),
                    symbol_count + MAX_HISTORY_DURATION,
                ));
                TransportState::Message(Ok(msg))
            }
            Some(Err(err)) => {
                debug!("assembler: gave up on bad decode: {}", err);
                TransportState::Message(Err(err))
            }
            _ => {
                if self.history.is_empty() {
                    TransportState::Idle
                } else {
                    TransportState::Assembling
                }
            }
        }
    }

    /// Iterator over bursts stored in the history
    #[inline]
    fn bursts(&self) -> impl ExactSizeIterator<Item = &[u8]> {
        self.history.iter().map(|td| td.data.as_slice())
    }

    /// Deduplicate a message result
    ///
    /// If the given result `res` is a duplicate of the last message, and
    /// the duplicate buffer has not yet timed out, then suppress it.
    fn deduplicate(&self, res: Option<MessageResult>) -> Option<MessageResult> {
        let res = res?;
        match res {
            Ok(msg) if self.is_not_duplicate(&msg) => Some(Ok(msg)),
            Ok(msg) => {
                debug!("assembler: suppressed duplicate message \"{}\"", msg);
                None
            }
            _ => Some(res),
        }
    }

    /// Messages are considered "duplicate" if they are string equal
    fn is_not_duplicate(&self, other: &Message) -> bool {
        if let Some(prev) = &self.previous {
            prev.as_ref().as_str() != other.as_str()
        } else {
            true
        }
    }
}

impl Default for Assembler {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents a message result maybe stored for later
#[derive(Clone, Debug, PartialEq, Eq)]
enum PendingResult {
    /// No complete messages are stored
    Empty,

    /// Attached result will issue when its deadline elapses
    Pending(TimedData<MessageResult>),
}

impl PendingResult {
    /// Store the given message as `Pending` if it's "better"
    ///
    /// Takes ownership of the given `msg`, assigns it an
    /// appropriate deadline based on the current symbol
    /// count `now`, and stores it if it is "better" than the
    /// existing message in this object—if any.
    ///
    /// Returns true if the new message was stored or false
    /// if it was discarded.
    pub fn accept(&mut self, msg: MessageResult, now: u64) -> bool {
        // EOMs are ready immediately. Everything else must wait
        let new = match msg {
            Ok(Message::EndOfMessage) => TimedData::with_deadline(msg, now),
            _ => TimedData::with_deadline(msg, now + MAX_INTERBURST_SYMBOLS),
        };

        if let PendingResult::Pending(old) = self {
            let replace_with_new_msg = match (&old.data, &new.data) {
                // no error is better than error
                (Err(_), _) => true,
                // start of message better than end of message
                (Ok(Message::EndOfMessage), Ok(Message::StartOfMessage(_m))) => true,
                // replace a start-of-message unless the old one is "better"
                (Ok(Message::StartOfMessage(old)), Ok(Message::StartOfMessage(new))) => {
                    new.voting_byte_count() >= old.voting_byte_count()
                }
                _ => false,
            };
            if replace_with_new_msg {
                debug!(
                    "assembler: replacing pending \"{:?}\" with \"{:?}\"",
                    old.data, new.data
                );
                *old = new;
                true
            } else {
                false
            }
        } else {
            debug!("assembler: pending: \"{:?}\"", new);
            *self = PendingResult::Pending(new);
            true
        }
    }

    /// If a message is ready "now," return it
    ///
    /// Checks the deadline of the `Pending` message (if any)
    /// against the current symbol counter `now`. If a message or
    /// error is ready to emit, removes and returns it. Returns
    /// `None` if no message is ready.
    pub fn poll(&mut self, now: u64) -> Option<MessageResult> {
        match self {
            Self::Pending(timed_result) if timed_result.is_expired_at(now) => {
                let out = Some(timed_result.data.clone());
                *self = Self::Empty;
                out
            }
            _ => None,
        }
    }
}

impl Default for PendingResult {
    fn default() -> Self {
        Self::Empty
    }
}

/// Burst history type
type BurstHistory = std::collections::VecDeque<TimedData<Burst>>;

/// Previous message type
type PreviousMessage = TimedData<Message>;

// Remove expired bursts from the history
#[inline]
fn prune_history(history: &mut BurstHistory, symbol_count: u64) {
    history.retain(|entry| !entry.is_expired_at(symbol_count));

    while history.len() > 2 {
        drop(history.pop_front());
    }
}

#[inline]
fn prune_previous(previous: &mut Option<PreviousMessage>, symbol_count: u64) {
    match previous {
        Some(msg) if msg.is_expired_at(symbol_count) => *previous = None,
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use super::super::waveform;
    use super::*;
    use crate::MessageDecodeErr;

    const ONE_SECOND: u64 = waveform::BAUD_HZ as u64;
    const BURST_TIMEOUT: u64 = (1.31 * waveform::BAUD_HZ) as u64;
    const ALMOST_TIMEOUT: u64 = (1.2 * waveform::BAUD_HZ) as u64;

    const TEST_EOM: &[u8] = "NNNN".as_bytes();
    const TEST_MSG_GOOD: &[u8] = "ZCZC-EAS-DMO-999000+0015-0011122-NOCALL00-".as_bytes();
    const TEST_MSG_ERRS: &[u8] = "ZCZK-EAS-DMF-999!00+0015-0011122-NOCALL00-KXYZ".as_bytes();
    const TEST_MSG_LONGEST: &[u8] = "ZCZC-EAS-DMO-372088-091724-919623-645687-745748-175234-039940-955869-091611-304171-931612-334828-179485-569615-809223-830187-611340-014693-472885-084645-977764-466883-406863-390018-701741-058097-752790-311648-820127-255900-581947+0000-0001122-NOCALL00-".as_bytes();

    fn history_times(history: &BurstHistory) -> Vec<u64> {
        history.iter().map(|elem| elem.deadline).collect()
    }

    // pre-burst delay and burst → burst with full timing information
    fn simulate_bursts<'a, I>(
        time: &'a mut u64,
        src: I,
    ) -> impl Iterator<Item = (usize, (u64, &'a [u8]))>
    where
        I: IntoIterator<Item = &'a (u64, &'a [u8])>,
    {
        src.into_iter()
            .map(move |(delay, data)| {
                *time = *time + 8 * data.len() as u64 + delay;
                if !data.is_empty() {
                    *time += 16 * 8; // include time for preamble
                }
                (*time, *data)
            })
            .enumerate()
    }

    #[test]
    fn test_prune_history() {
        let mut history = BurstHistory::default();
        history.push_back(TimedData::with_deadline(Burst::default(), 1));
        history.push_back(TimedData::with_deadline(Burst::default(), 2144));
        history.push_back(TimedData::with_deadline(Burst::default(), 3000));

        // note: expiration time is about 5642 symbols

        prune_history(&mut history, 0);
        assert_eq!(history.len(), 2);

        prune_history(&mut history, 2143);
        assert_eq!(&[2144u64, 3000], history_times(&history).as_slice());

        prune_history(&mut history, 2999);
        assert_eq!(&[3000u64], history_times(&history).as_slice());

        prune_history(&mut history, 6000);
        assert!(history.is_empty());
    }

    #[test]
    fn test_pending_result() {
        const NO_ERRORS: &[u8] = &[0u8; TEST_MSG_GOOD.len()];
        const VOTING_NONE: &[u8] = &[2u8; TEST_MSG_GOOD.len()];
        const VOTING_ALL: &[u8] = &[3u8; TEST_MSG_GOOD.len()];

        let test_message_novoting = Message::try_from((TEST_MSG_GOOD, NO_ERRORS, VOTING_NONE));
        assert!(test_message_novoting.is_ok());
        let test_message_voting = Message::try_from((TEST_MSG_GOOD, NO_ERRORS, VOTING_ALL));
        assert!(test_message_voting.is_ok());

        let mut uut = PendingResult::default();

        // errors replace nothing, or errors
        assert!(uut.accept(Err(MessageDecodeErr::NotAscii), 0));
        assert!(uut.accept(Err(MessageDecodeErr::UnrecognizedPrefix), 0));

        // not yet ready
        assert_eq!(None, uut.poll(0));

        // an EOM replaces it
        assert!(uut.accept(Ok(Message::EndOfMessage), 0));

        // EOM does not replace EOM
        assert!(!uut.accept(Ok(Message::EndOfMessage), 0));

        // EOM is ready immediately
        assert_eq!(Some(Ok(Message::EndOfMessage)), uut.poll(0));
        assert_eq!(PendingResult::default(), uut);

        // Crappy estimate accepted
        assert!(uut.accept(test_message_novoting.clone(), 0));

        // Can be replaced with itself
        assert!(uut.accept(test_message_novoting.clone(), 0));

        // Not ready immediately
        assert_eq!(None, uut.poll(0));

        // Replaced with a message that has more voting, which
        // we also receive later
        assert!(uut.accept(test_message_voting.clone(), 5650));

        // not replaced with EOM, error, or message with less voting
        assert!(!uut.accept(Ok(Message::EndOfMessage), 5650));
        assert!(!uut.accept(Err(MessageDecodeErr::NotAscii), 5650));
        assert!(!uut.accept(test_message_novoting.clone(), 5650));

        // still not ready immediately
        assert_eq!(None, uut.poll(5650));

        // but is ready in the future
        let out = uut.poll(2 * 5650);
        assert_eq!(Some(test_message_voting), out);
    }

    #[test]
    fn test_assembler_deduplicate() {
        // Four EOMs close together, and a fifth after some time
        const FOUR_EOM: &[(u64, &[u8])] = &[
            (999 * ONE_SECOND, &[]),
            (0, TEST_EOM),
            (ONE_SECOND, TEST_EOM),
            (ONE_SECOND, TEST_EOM),
            (12 * ONE_SECOND, TEST_EOM),
        ];

        let mut assembler = Assembler::default();
        let mut time = 0u64;
        let mut ok = 0i32;
        for (itr, (tm, data)) in simulate_bursts(&mut time, FOUR_EOM.iter()) {
            match (itr, assembler.assemble(data, tm), &assembler.state) {
                (0, TransportState::Idle, PendingResult::Empty) => {
                    // nothing happening yet
                    ok += 1;
                }
                (1, TransportState::Message(Ok(Message::EndOfMessage)), PendingResult::Empty) => {
                    // we fast EOM immediately
                    ok += 1;
                }
                (2, TransportState::Assembling, PendingResult::Empty) => {
                    // duplicate suppression prevents us from considering
                    ok += 1;
                }
                (3, TransportState::Assembling, PendingResult::Empty) => {
                    // duplicate suppression prevents us from considering
                    ok += 1;
                }
                (4, TransportState::Message(Ok(Message::EndOfMessage)), PendingResult::Empty) => {
                    // the duplicate expired
                    ok += 1;
                }
                _ => {
                    unreachable!()
                }
            }
        }
        assert_eq!(ok, 5);
    }

    #[test]
    fn test_assembler_normal_operation() {
        // Classic full SOM followed by full EOM, with voting for error-correction
        // This is what we expect to receive when all is working.
        const SOM_EOM: &[(u64, &[u8])] = &[
            (0, TEST_MSG_GOOD),
            (ONE_SECOND, &[]),
            (0, TEST_MSG_GOOD),
            (ONE_SECOND, &[]),
            (0, TEST_MSG_ERRS),
            (BURST_TIMEOUT, &[]),
            (15 * ONE_SECOND, TEST_EOM),
            (ONE_SECOND, TEST_EOM),
            (ONE_SECOND, TEST_EOM),
        ];

        let mut assembler = Assembler::default();
        let mut time = 0u64;
        let mut ok = 0i32;

        for (itr, (tm, data)) in simulate_bursts(&mut time, SOM_EOM.iter()) {
            let out = assembler.assemble(data, tm);
            match (itr, out, &assembler.state) {
                (0, TransportState::Assembling, PendingResult::Empty) => {
                    // Message goes into buffer
                    ok += 1;
                }
                (1, TransportState::Assembling, PendingResult::Empty) => {
                    // no new data
                    ok += 1;
                }
                (2, TransportState::Assembling, PendingResult::Pending(_)) => {
                    // message becomes pending
                    ok += 1;
                }
                (3, TransportState::Assembling, PendingResult::Pending(_)) => {
                    // still not ready yet
                    ok += 1;
                }
                (4, TransportState::Assembling, PendingResult::Pending(_)) => {
                    // got a new message and kept it
                    ok += 1;
                }
                (
                    5,
                    TransportState::Message(Ok(Message::StartOfMessage(hdr))),
                    PendingResult::Empty,
                ) => {
                    // message emits
                    assert_eq!(hdr.voting_byte_count(), TEST_MSG_GOOD.len());
                    ok += 1;
                }
                (6, TransportState::Message(Ok(Message::EndOfMessage)), PendingResult::Empty) => {
                    // fast eom
                    ok += 1;
                }
                (7, TransportState::Assembling, PendingResult::Empty) => {
                    // eom duplicate-suppressed
                    ok += 1;
                }
                (8, TransportState::Assembling, PendingResult::Empty) => {
                    // eom duplicate-suppressed
                    ok += 1;
                }
                _ => {
                    unreachable!()
                }
            }
        }

        assert_eq!(ok, 9);
    }

    #[test]
    fn test_assembler_very_long_message() {
        // Classic SOM, with the longest possible message and big timing gaps
        const LONG_MSGS: &[(u64, &[u8])] = &[
            (0, TEST_MSG_LONGEST),
            (ALMOST_TIMEOUT, &[]),
            (0, TEST_MSG_LONGEST),
            (ALMOST_TIMEOUT, &[]),
            (0, TEST_MSG_LONGEST),
            (BURST_TIMEOUT, &[]),
        ];

        let mut assembler = Assembler::default();
        let mut time = 0u64;
        let mut ok = 0i32;

        for (itr, (tm, data)) in simulate_bursts(&mut time, LONG_MSGS.iter()) {
            let out = assembler.assemble(data, tm);
            match (itr, out, &assembler.state) {
                (0, TransportState::Assembling, PendingResult::Empty) => {
                    // Message goes into buffer
                    ok += 1;
                }
                (1, TransportState::Assembling, PendingResult::Empty) => {
                    // Message still in buffer
                    ok += 1;
                }
                (2, TransportState::Assembling, PendingResult::Pending(_)) => {
                    // Have enough for a message, but don't emit it yet
                    ok += 1;
                }
                (3, TransportState::Assembling, PendingResult::Pending(_)) => {
                    // Have enough for a message, but don't emit it yet
                    ok += 1;
                }
                (4, TransportState::Assembling, PendingResult::Pending(_)) => {
                    // Have enough for a voting message, but don't emit it yet
                    ok += 1;
                }
                (
                    5,
                    TransportState::Message(Ok(Message::StartOfMessage(hdr))),
                    PendingResult::Empty,
                ) => {
                    // Now ready
                    assert_eq!(hdr.voting_byte_count(), TEST_MSG_LONGEST.len());
                    assert_eq!(hdr.as_str().as_bytes(), TEST_MSG_LONGEST);
                    ok += 1;
                }
                _ => unreachable!(),
            }
        }

        assert_eq!(ok, 6);
    }

    #[test]
    fn test_assembler_very_long_message_missing_middle() {
        // Classic SOM, with the longest possible message.
        // The middle burst is missing.
        const LONG_MSGS: &[(u64, &[u8])] = &[
            (0, TEST_MSG_LONGEST),
            (ALMOST_TIMEOUT, &[]),
            (268 * 8, &[]),
            (ALMOST_TIMEOUT, &[]),
            (0, TEST_MSG_LONGEST),
            (BURST_TIMEOUT, &[]),
        ];

        let mut assembler = Assembler::default();
        let mut time = 0u64;
        let mut ok = 0i32;

        for (itr, (tm, data)) in simulate_bursts(&mut time, LONG_MSGS.iter()) {
            let out = assembler.assemble(data, tm);
            match (itr, out, &assembler.state) {
                (4, TransportState::Assembling, PendingResult::Pending(_)) => {
                    // Message goes into buffer
                    ok += 1;
                }
                (
                    5,
                    TransportState::Message(Ok(Message::StartOfMessage(hdr))),
                    PendingResult::Empty,
                ) => {
                    // Now ready, no voting
                    assert_eq!(hdr.voting_byte_count(), 0);
                    assert_eq!(hdr.as_str().as_bytes(), TEST_MSG_LONGEST);
                    ok += 1;
                }

                (_, TransportState::Assembling, PendingResult::Empty) => {
                    // Message goes into buffer
                    ok += 1;
                }
                _ => {
                    unreachable!()
                }
            }
        }

        assert_eq!(ok, 6);
    }

    #[test]
    fn test_assembler_quickly_with_missing() {
        // Two EOMs followed immediately by SOM and EOM
        // This is a very tricky combination!
        const TWO_EOM_AND_MSG: &[(u64, &[u8])] = &[
            (0, TEST_EOM),
            (ONE_SECOND, TEST_EOM),
            (ONE_SECOND, TEST_MSG_GOOD),
            ((1.1 * ONE_SECOND as f32) as u64, TEST_MSG_GOOD),
            (BURST_TIMEOUT, &[]),
            (ONE_SECOND, TEST_EOM),
            (ONE_SECOND, TEST_EOM),
        ];

        let mut assembler = Assembler::default();
        let mut time = 0u64;
        let mut ok = 0i32;

        for (itr, (tm, data)) in simulate_bursts(&mut time, TWO_EOM_AND_MSG.iter()) {
            match (itr, assembler.assemble(data, tm), &assembler.state) {
                (0, TransportState::Message(Ok(Message::EndOfMessage)), PendingResult::Empty) => {
                    // Fast EOM
                    ok += 1;
                }
                (1, TransportState::Assembling, PendingResult::Empty) => {
                    // EOM, duplicate-suppressed
                    ok += 1;
                }
                (2, TransportState::Assembling, PendingResult::Empty) => {
                    // get ZCZC, but outvoted in favor of NNNN,
                    // which gets duplicate-suppressed
                    ok += 1;
                }
                (3, TransportState::Assembling, PendingResult::Pending(_x)) => {
                    // get ZCZC, and we vote in favor of it
                    ok += 1;
                }
                (
                    4,
                    TransportState::Message(Ok(Message::StartOfMessage(msg))),
                    PendingResult::Empty,
                ) => {
                    // the ZCZC is now ready
                    assert_eq!(msg.voting_byte_count(), 4);
                    ok += 1;
                }
                (5, TransportState::Assembling, PendingResult::Empty) => {
                    // buffer now contains two ZCZC and an NNNN
                    // the result is a ZCZC, but it gets duplicate-suppressed
                    ok += 1;
                }
                (6, TransportState::Message(Ok(Message::EndOfMessage)), PendingResult::Empty) => {
                    // ZCZC outvoted in favor of NNNN, emitted
                    ok += 1;
                }
                _ => {
                    unreachable!()
                }
            }
        }

        assert_eq!(ok, 7);
    }
}
