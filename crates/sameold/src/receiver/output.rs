use crate::message::{Message, MessageResult};

/// Full SAME receiver status
///
/// SAME decoding occurs at two separate layers:
///
/// 1. **Link layer**: the demodulator, synchronizer, and framer
///    convert analog waveforms into *frames* of data. Each
///    individual SAME "burst" is a frame.
///
/// 2. **Transport layer**: each SAME frame is transmitted three
///    times for redundancy. These retransmissions are combined
///    together to form a single [`Message`] estimate.
///
/// The [`what()`](SameReceiverEvent::what) method returns the event,
/// which may originate from either layer.
///
/// You can also query for the
/// [`message()`](SameReceiverEvent::message) or an individual
/// [`burst()`](SameReceiverEvent::burst) estimate, if one is
/// available now. Not all events have either of these things
/// to report.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SameReceiverEvent {
    what: SameEventType,
    input_sample_counter: u64,
}

impl SameReceiverEvent {
    /// Message or message decoding error, if any
    ///
    /// If the current update includes a message or a reportable
    /// decoding error, returns `Some()` with a
    /// [message result](MessageResult).
    pub fn message(&self) -> Option<&MessageResult> {
        match self.what() {
            SameEventType::Transport(TransportState::Message(res)) => Some(res),
            _ => None,
        }
    }

    /// Successfully-decoded message, if any
    ///
    /// If the current update includes a successfully-decoded
    /// message, returns it. If no message is available, returns
    /// `None`. Consider using
    /// [`message()`](SameReceiverEvent::message()) instead to
    /// report errors.
    pub fn message_ok(&self) -> Option<&Message> {
        match self.what() {
            SameEventType::Transport(TransportState::Message(res)) => res.as_ref().ok(),
            _ => None,
        }
    }

    /// Raw SAME data burst, if any
    ///
    /// Digital SAME headers are transmitted three times for
    /// redundancy. This method retrieves the raw received bytes
    /// from the link layer if any are available with this event.
    ///
    /// If present, the link layer guarantees that:
    ///
    /// 1. No further bytes will be added to this burst. It is
    ///    complete as far as the modem is aware.
    ///
    /// 2. The bytes are aligned to the start of the transmission
    ///    after the preamble. If the bytes were received
    ///    error-free, they begin with `ZCZC` or `NNNN`, but this
    ///    itself is not guaranteed.
    ///
    /// SAME messages use only ASCII printable characters, but
    /// **no such guarantees** are made about this field.
    ///
    /// Clients **MUST NOT** report a single burst as a
    /// SAME message. Messages should instead be obtained from
    /// from the transport layer's [`message()`](SameReceiverEvent::message).
    pub fn burst(&self) -> Option<&[u8]> {
        match self.what() {
            SameEventType::Link(LinkState::Burst(res)) => Some(res.as_ref()),
            _ => None,
        }
    }

    /// Consume event, returning successfully-decoded message, if any
    ///
    /// If the current update includes a successfully-decoded
    /// message, returns it. If no message is available, returns
    /// `None`.
    pub fn into_message(self) -> Option<MessageResult> {
        match self.what {
            SameEventType::Transport(TransportState::Message(res)) => Some(res),
            _ => None,
        }
    }

    /// Consume event, returning successfully-decoded message, if any
    ///
    /// If the current update includes a successfully-decoded
    /// message, returns it. If no message is available, returns
    /// `None`. Consider using [`SameReceiverEvent::into_message`] instead.
    pub fn into_message_ok(self) -> Option<Message> {
        match self.what {
            SameEventType::Transport(TransportState::Message(res)) => res.ok(),
            _ => None,
        }
    }

    /// The event which triggered the output
    ///
    /// Either the link layer or the transport layer may trigger
    /// an event.
    pub fn what(&self) -> &SameEventType {
        &self.what
    }

    /// Event time, measured in input samples
    ///
    /// Reports the "time" of the event using a monotonic count
    /// of input samples.
    pub fn input_sample_counter(&self) -> u64 {
        self.input_sample_counter
    }
}

impl SameReceiverEvent {
    /// Create from event and time
    pub(crate) fn new<E>(what: E, input_sample_counter: u64) -> Self
    where
        E: Into<SameEventType>,
    {
        Self {
            what: what.into(),
            input_sample_counter,
        }
    }
}

impl From<SameReceiverEvent> for Option<MessageResult> {
    fn from(rx: SameReceiverEvent) -> Self {
        rx.into_message()
    }
}

impl From<SameReceiverEvent> for Option<Message> {
    fn from(rx: SameReceiverEvent) -> Self {
        rx.into_message_ok()
    }
}

impl std::fmt::Display for SameReceiverEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{:<14}]: event {}",
            self.input_sample_counter,
            self.what()
        )
    }
}

/// Type of event
///
/// See [`SameReceiverEvent`]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SameEventType {
    /// Link layer event
    ///
    /// Link layer events include acquisition and dropping of
    /// carrier and individual burst estimates
    Link(LinkState),

    /// Transport layer events
    ///
    /// Transport layer events include notifications about
    /// buffered messages which are being assembled and completely
    /// parsed [messages](Message) which are ready to present to
    /// the user.
    Transport(TransportState),
}

impl From<LinkState> for SameEventType {
    fn from(inp: LinkState) -> Self {
        Self::Link(inp)
    }
}

impl From<TransportState> for SameEventType {
    fn from(inp: TransportState) -> Self {
        Self::Transport(inp)
    }
}

impl AsRef<str> for SameEventType {
    fn as_ref(&self) -> &str {
        match self {
            SameEventType::Link(_) => "link",
            SameEventType::Transport(_) => "transport",
        }
    }
}

impl std::fmt::Display for SameEventType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SameEventType::Link(evt) => write!(f, "[{}]: {}", self.as_ref(), evt),
            SameEventType::Transport(evt) => write!(f, "[{}]: {}", self.as_ref(), evt),
        }
    }
}

/// SAME Link Layer Status
///
/// The SAME receiver decodes data in three basic steps:
///
/// 1. Synchronization: the receiver estimates the bit
///    and byte synchronization of the incoming signal.
///
/// 2. Burst decoding: SAME transmissions are repeated
///    three times. Each "burst" is decoded individually.
///
/// 3. Message framing: three bursts are assembled into
///    a single message, which receives some basic
///    validity checks before being emitted to the client.
///
/// The `LinkState` reflects the overall state of this
/// system as it detects the carrier, SAME preamble, and
/// reads a Burst.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum LinkState {
    /// Dropped data carrier
    NoCarrier,

    /// Searching for burst prefix
    ///
    /// The framer has synchronized to the bit/byte
    /// boundaries and is searching for a message prefix
    /// like "`ZCZC`."
    Searching,

    /// Now reading a burst
    ///
    /// A SAME burst has been detected and is now being
    /// decoded.
    Reading,

    /// A burst has been completely read
    ///
    /// The current burst has ended, and the raw burst
    /// bytes are included. SAME messages use only ASCII
    /// printable characters, but **no such guarantees** are
    /// made about the contents of this enum. The `Burst`
    /// is aligned to the start of the transmission ("`ZCZC`"
    /// or "`NNNN`"), but even those bytes may have errors.
    ///
    /// Clients **MUST NOT** report a single `Burst` as a
    /// SAME message. Messages should instead be obtained from
    /// from the transport layer's [`TransportState::Message`].
    Burst(Vec<u8>),
}

impl AsRef<str> for LinkState {
    fn as_ref(&self) -> &str {
        match self {
            LinkState::NoCarrier => "no carrier",
            LinkState::Searching => "searching",
            LinkState::Reading => "reading",
            LinkState::Burst(_) => "decoded burst",
        }
    }
}

impl std::fmt::Display for LinkState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkState::Burst(data) => write!(
                f,
                "{}: \"{}\"",
                self.as_ref(),
                String::from_utf8_lossy(&data)
            ),
            _ => write!(f, "{}", self.as_ref()),
        }
    }
}

/// SAME Transport Layer Status
///
/// SAME [messages](Message) may be built from one, two, or three
/// repetitions of (hopefully) identical retransmissions. Each
/// burst is passed to the transport layer for processing.
///
/// 1. When the transport layer receives its first burst, the state
///    becomes [`Assembling`](TransportState::Assembling).
///
/// 2. The modem waits for some time after the last burst is
///    received to ensure it has the best decode possible.
///
/// 3. After this deadline passes, and a sufficient number of
///    bursts are received, a [`Message`](TransportState::Message)
///    is reported. If the decode failed, this `Message` might be
///    an error.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum TransportState {
    /// The assembler has not read enough bursts for a message
    Idle,

    /// The assembler is building a message
    ///
    /// One or more bursts are stored in the history, but the
    /// assembler needs more before it can output a message.
    Assembling,

    /// A fully-assembled message or error is ready to report
    Message(MessageResult),
}

impl AsRef<str> for TransportState {
    fn as_ref(&self) -> &str {
        match self {
            TransportState::Idle => "idle",
            TransportState::Assembling => "assembling",
            TransportState::Message(Ok(_)) => "message",
            TransportState::Message(Err(_)) => "decode error",
        }
    }
}

impl std::fmt::Display for TransportState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TransportState::Message(Ok(msg)) => write!(
                f,
                "{}: ({:.1}% voting, {} errors) \"{}\"",
                self.as_ref(),
                100.0 * (msg.voting_byte_count() as f32 / msg.as_str().len() as f32),
                msg.parity_error_count(),
                msg
            ),
            TransportState::Message(Err(err)) => write!(f, "{}: \"{}\"", self.as_ref(), err),
            _ => write!(f, "{}", self.as_ref()),
        }
    }
}
