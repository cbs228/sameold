use crate::message::{Message, MessageDecodeErr};

/// SAME/EAS Receiver Status
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
/// This enum reports changes in the framing state to the
/// caller. Each state change is emitted individually.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FrameOut {
    /// No signal detected
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

    /// A message has been completely read
    ///
    /// The current burst has ended, and the framer
    /// attempted to frame a complete message. It
    /// succeeded if the value is `Ok`.
    ///
    /// The result contains either a fully-decoded
    /// [`Message`](crate::Message) that is
    /// ready for presentation to the user *or* an
    /// error decoding the same. Errors indicate only
    /// that decoding has failed *for the moment*, and
    /// future decodes may yield a useful message.
    Ready(Result<Message, MessageDecodeErr>),
}

impl FrameOut {
    /// True if the Framer wants data
    ///
    /// Returns `true` if the framer is either searching for
    /// a burst prefix or reading burst data. When this value
    /// is `true`, the framer wants data. When this value is
    /// `false`, the framer needs to be restarted before it
    /// will do anything.
    pub fn is_active(&self) -> bool {
        match self {
            FrameOut::Searching => true,
            FrameOut::Reading => true,
            _ => false,
        }
    }
}

impl AsRef<str> for FrameOut {
    fn as_ref(&self) -> &str {
        match self {
            FrameOut::NoCarrier => "no carrier",
            FrameOut::Searching => "searching",
            FrameOut::Reading => "reading",
            FrameOut::Ready(_) => "message ready",
        }
    }
}

impl std::fmt::Display for FrameOut {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FrameOut::Ready(Ok(msg)) => write!(f, "{}: \"{}\"", self.as_ref(), msg),
            FrameOut::Ready(Err(e)) => write!(f, "{}: error: {}", self.as_ref(), e),
            _ => write!(f, "{}", self.as_ref()),
        }
    }
}
