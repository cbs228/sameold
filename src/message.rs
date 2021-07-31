//! Framer outputs for the client

use std::convert::TryFrom;
use std::fmt;

use lazy_static::lazy_static;
use regex::Regex;

/// A fully-decoded SAME/EAS message
///
/// In the EAS, the "message" is actually the audio signal to be
/// broadcast to the human listener: i.e., the "message" is the
/// synthesized voice you hear on weather radio. The message is
/// wrapped in *audio pass-band* digital data. The digital data
/// demarcates the `StartOfMessage` and the `EndOfMessage`.
///
/// The `StartOfMessage` contains digital codes and timestamps
/// which summarize the audio message to follow. Some messages
/// are intended for either silent or audible tests. Others
/// report actual emergencies; these either may or must interrupt
/// normal broadcast programming.
///
/// The audio message immediately follows. The audio message may
/// be up to two minutes long.
///
/// The `EndOfMessage` demarcates the end of the audio message.
///
/// More information on the SAME/EAS standard may be found in,
/// * "NOAA Weather Radio (NWR) All Hazards Specific Area Message
///   Encoding (SAME)," NWSI 10-172, 3 Oct. 2011,
///   <https://www.nws.noaa.gov/directives/sym/pd01017012curr.pdf>
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Message {
    /// Indicates start of audio message
    ///
    /// A `StartOfMessage` indicates that a SAME/EAS audio
    /// message immediately follows. The message
    /// [header](struct.MessageHeader.html) contains the event
    /// type, affected areas, time extents, and originator
    /// information.
    ///
    /// For broadcast stations, the in-band audio which immediately
    /// follows the `StartOfMessage` *may* break station
    /// programming and be aired directly to listeners.
    StartOfMessage(MessageHeader),

    /// Indicates end of audio message
    ///
    /// An `EndOfMessage` marks the conclusion of the SAME/EAS
    /// audio message. For broadcast stations, it is an
    /// indication that normal programming may resume.
    EndOfMessage,
}

/// Error decoding a `MessageHeader`
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MessageDecodeErr {
    /// The starting prefix of the message was not recognized
    UnrecognizedPrefix,

    /// Header contains non-ASCII characters
    NotAscii,

    /// Header is shorter than the minimum length for a valid message
    TooShort,

    /// Header does not match general format
    Malformed,
}

impl Message {
    /// Convert to string representation
    pub fn as_str(&self) -> &str {
        match self {
            Self::StartOfMessage(m) => m.message(),
            Self::EndOfMessage => PREFIX_MESSAGE_END,
        }
    }
}

/// Event, area, time, and originator information
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MessageHeader {
    // message content, including the leading `ZCZC-`
    message: String,

    // where does the time field begin?
    // includes the leading plus character (`+`)
    offset_time: usize,

    // parity errors
    parity_error_count: usize,
}

impl MessageHeader {
    /// Try to construct a SAME header from `String`
    ///
    /// The `message` string must match the general format of
    /// a SAME header. If it does not, an error is returned.
    pub fn new<S>(message: S) -> Result<Self, MessageDecodeErr>
    where
        S: Into<String>,
    {
        let mut message: String = message.into();
        if !message.is_ascii() {
            return Err(MessageDecodeErr::NotAscii);
        }

        let (offset_time, hdr_length) = check_header(&message)?;
        message.truncate(hdr_length);

        Ok(Self {
            message,
            offset_time,
            parity_error_count: 0,
        })
    }

    /// Try to construct a SAME header from `String`, with error count
    ///
    /// The `message` string must match the general format of
    /// a SAME header. If it does not, an error is returned. The
    /// `parity_errors` field is intended to store the number of
    /// *bit errors* which were corrected by the 2-of-3 parity
    /// algorithm.
    pub fn new_with_error_count<S>(
        message: S,
        parity_errors: usize,
    ) -> Result<Self, MessageDecodeErr>
    where
        S: Into<String>,
    {
        let mut out = Self::new(message)?;
        out.parity_error_count = parity_errors;
        Ok(out)
    }
    /// Message text
    ///
    /// Returns UTF-8 string representation of a SAME/EAS
    /// message. Use the [`release()`](#method.release)
    /// method to obtain an owned `String`.
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Originator code
    ///
    /// A three-character string that is usually one of the
    /// following:
    ///
    /// - `PEP`: Primary Entry Point Station. Generally only
    ///   used for national activations, which are very rare.
    ///
    /// - `CIV`: Civil authorities (usu. state and local government)
    ///
    /// - `WXR`: National Weather Service or Environment Canada
    ///
    /// - `EAS`: EAS Participant. Usually a broadcast station.
    ///
    /// - `EAN`: Emergency Action Notification Network.
    ///   Nation-wide activation authorized by the President of
    ///   the United States. Takes priority over all other
    ///   messages/station programming.
    ///
    /// The originator code returned is three characters but is
    /// not guaranteed to be one of the above.
    pub fn originator(&self) -> &str {
        &self.message[Self::OFFSET_ORG..Self::OFFSET_ORG + 3]
    }

    /// Event code
    ///
    /// A three-character code which is generally formatted
    /// according to severity level.
    ///
    /// - `xxT`: Test
    /// - `xxS`: Statement / Advisory
    /// - `xxA`: Watch
    /// - `xxW`: Warning (generally most severe events)
    ///
    /// Major exceptions to this are the codes `SVR`
    /// ("Severe Thunderstorm Warning") and `TOR`
    /// ("Tornado Warning"), which are among the most common
    /// messages in the United States.
    ///
    /// The event code returned is three characters but is
    /// not guaranteed to be one of the above.
    pub fn event(&self) -> &str {
        &self.message[Self::OFFSET_EVT..Self::OFFSET_EVT + 3]
    }

    /// Iterator over location codes
    ///
    /// Returns an iterator over the location codes in the
    /// message. Location codes are six-digit strings of
    /// the form `PSSCCC`:
    ///
    /// - `P`: part of county, or zero for entire county
    /// - `SS`: FIPS State code
    /// - `CCC`: FIPS County code
    ///
    /// Locations are returned in the order listed in the
    /// message. Iterator values are guaranteed to be
    /// six-digit strings.
    ///
    /// Per the SAME standard, a message can have up to 31
    /// location codes.
    pub fn location_iter<'m>(&'m self) -> std::str::Split<'m, char> {
        let locations = &self.message[Self::OFFSET_AREA_START..self.offset_time];
        locations.split('-')
    }

    /// Message validity duration (as string)
    ///
    /// Returns the message validity duration or "purge time."
    /// This is a four-character string of "`HHMM`" representing
    /// a duration in hours and minutes.
    ///
    /// This field represents the validity time of the *message*
    /// and not the expected duration of the severe condition.
    /// Severe conditions may persist after the message expires!
    /// (And might be the subject of future messages.)
    ///
    /// The valid duration is relative to the
    /// [`issue_daytime()`](#method.issue_daytime).
    ///
    /// The returned string is guaranteed to be a four-digit
    /// number.
    pub fn valid_duration_str(&self) -> &str {
        &self.message[self.offset_time + Self::OFFSET_FROMPLUS_VALIDTIME
            ..self.offset_time + Self::OFFSET_FROMPLUS_VALIDTIME + 4]
    }

    /// Message validity duration
    ///
    /// Returns the message validity duration or "purge time."
    /// This is a tuple of (`hours`, `minutes`).
    ///
    /// This field represents the validity time of the *message*
    /// and not the expected duration of the severe condition.
    /// Severe conditions may persist after the message expires!
    /// (And might be the subject of future messages.)
    ///
    /// The valid duration is relative to the
    /// [`issue_daytime()`](#method.issue_daytime).
    pub fn valid_duration(&self) -> (u8, u8) {
        let dur_str = self.valid_duration_str();
        (
            dur_str[0..2].parse().expect(Self::PANIC_MSG),
            dur_str[2..4].parse().expect(Self::PANIC_MSG),
        )
    }

    /// Mesage issuance day/time (string)
    ///
    /// Returns the message issue day and time, as the string
    /// `JJJHHMM`,
    ///
    /// - `JJJ`: Ordinal day of the year. `001` represents 1 Jan.,
    ///   and `365` represents 31 Dec. in non leap-years. During
    ///   leap years, `366` represents 31 Dec. `000` is not used.
    ///   It is up to the receiving station to have some notion
    ///   of what the current year is and to detect calendar
    ///   rollovers.
    ///
    /// - `HHMM`: UTC time of day, using a 24-hour time scale.
    ///   Times are UTC and are **NOT** local times.
    ///
    /// The string format is guaranteed at the return, but the
    /// represented date is not guaranteed to be valid.
    pub fn issue_daytime_str(&self) -> &str {
        &self.message[self.offset_time + Self::OFFSET_FROMPLUS_ISSUETIME
            ..self.offset_time + Self::OFFSET_FROMPLUS_ISSUETIME + 7]
    }

    /// Mesage issuance day/time (string)
    ///
    /// Returns the message issue day and time, as the string
    /// `JJJHHMM`,
    ///
    /// - `JJJ`: Ordinal day of the year. `001` represents 1 Jan.,
    ///   and `365` represents 31 Dec. in non leap-years. During
    ///   leap-years, `366` represents 31 Dec. `000` is not used.
    ///   It is up to the receiving station to have some notion
    ///   of what the current year is and to detect calendar
    ///   rollovers.
    ///
    /// - `HHMM`: UTC time of day, using a 24-hour time scale.
    ///   Times are UTC and are **NOT** local times.
    pub fn issue_daytime(&self) -> (u16, u8, u8) {
        let issue = self.issue_daytime_str();
        (
            issue[0..3].parse().expect(Self::PANIC_MSG),
            issue[3..5].parse().expect(Self::PANIC_MSG),
            issue[5..7].parse().expect(Self::PANIC_MSG),
        )
    }

    /// Sending station callsign
    ///
    /// The FCC or other regulatory body-assigned callsign
    /// of the sending station. Minus signs (`-`) in the
    /// callsign are replaced with slashes (`/`). This field
    /// is always eight characters long.
    pub fn callsign(&self) -> &str {
        let end = self.message.len();
        &self.message[end - Self::OFFSET_FROMEND_CALLSIGN..end - Self::OFFSET_FROMEND_CALLSIGN_END]
    }

    /// Count of parity errors
    ///
    /// The number of *bit errors* which were corrected by the
    /// 2-of-3 parity correction algorithm. High parity error
    /// counts indicate a high bit error rate in the receiving
    /// system.
    pub fn parity_error_count(&self) -> usize {
        self.parity_error_count
    }

    /// Obtain the owned message String
    ///
    /// Destroys this object and releases the message
    /// contained within
    pub fn release(self) -> String {
        self.message
    }

    const OFFSET_ORG: usize = 5;
    const OFFSET_EVT: usize = 9;
    const OFFSET_AREA_START: usize = 13;
    const OFFSET_FROMPLUS_VALIDTIME: usize = 1;
    const OFFSET_FROMPLUS_ISSUETIME: usize = 6;
    const OFFSET_FROMEND_CALLSIGN: usize = 9;
    const OFFSET_FROMEND_CALLSIGN_END: usize = 1;
    const PANIC_MSG: &'static str = "MessageHeader validity check admitted a malformed message";
}

impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Message::StartOfMessage(msg) => msg.fmt(f),
            Message::EndOfMessage => PREFIX_MESSAGE_END.fmt(f),
        }
    }
}

impl AsRef<str> for Message {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl TryFrom<String> for Message {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: String) -> Result<Self, Self::Error> {
        if inp.starts_with(PREFIX_MESSAGE_START) {
            Ok(Message::StartOfMessage(MessageHeader::try_from(inp)?))
        } else if inp.starts_with(PREFIX_MESSAGE_END) {
            Ok(Message::EndOfMessage)
        } else {
            Err(MessageDecodeErr::UnrecognizedPrefix)
        }
    }
}

impl TryFrom<(String, usize)> for Message {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: (String, usize)) -> Result<Self, Self::Error> {
        if inp.0.starts_with(PREFIX_MESSAGE_START) {
            Ok(Message::StartOfMessage(MessageHeader::try_from(inp)?))
        } else if inp.0.starts_with(PREFIX_MESSAGE_END) {
            Ok(Message::EndOfMessage)
        } else {
            Err(MessageDecodeErr::UnrecognizedPrefix)
        }
    }
}

impl fmt::Display for MessageHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl AsRef<str> for MessageHeader {
    #[inline]
    fn as_ref(&self) -> &str {
        self.message()
    }
}

impl AsRef<[u8]> for MessageHeader {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.message().as_bytes()
    }
}

impl From<MessageHeader> for String {
    #[inline]
    fn from(msg: MessageHeader) -> String {
        msg.release()
    }
}

impl TryFrom<String> for MessageHeader {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: String) -> Result<Self, Self::Error> {
        Self::new(inp)
    }
}

impl TryFrom<(String, usize)> for MessageHeader {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: (String, usize)) -> Result<Self, Self::Error> {
        Self::new_with_error_count(inp.0, inp.1)
    }
}

impl fmt::Display for MessageDecodeErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MessageDecodeErr::UnrecognizedPrefix => {
                write!(f, "invalid SAME header: unrecognized prefix")
            }
            MessageDecodeErr::NotAscii => {
                write!(
                    f,
                    "invalid SAME header: message contains non-ASCII characters"
                )
            }
            MessageDecodeErr::TooShort => {
                write!(f, "invalid SAME header: decoded message too short")
            }
            MessageDecodeErr::Malformed => {
                write!(
                    f,
                    "invalid SAME header: message text does not match required pattern"
                )
            }
        }
    }
}

impl std::error::Error for MessageDecodeErr {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

const PREFIX_MESSAGE_START: &str = "ZCZC-";
const PREFIX_MESSAGE_END: &str = "NNNN";

// Check message header for basic format compliance
//
// We validate that the message may be split into fields
// correctly, but we do *not* do much validation of the
// fields themselves. Returns tuple of
//
// 1. start position of the purge time field, following
//    the `+`.
// 2. total length of the header. The `hdr` may be longer.
fn check_header(hdr: &str) -> Result<(usize, usize), MessageDecodeErr> {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"^ZCZC-[A-Z]{3}-[A-Z]{3}(-[0-9]{6})+(\+[0-9]{4}-[0-9]{7}-.{8}-)")
                .expect("bad SAME regexp");
    }

    if hdr.len() < 42 {
        return Err(MessageDecodeErr::TooShort);
    }

    let mtc = RE
        .captures(hdr)
        .ok_or(MessageDecodeErr::Malformed)?
        .get(2)
        .ok_or(MessageDecodeErr::Malformed)?;

    Ok((mtc.start(), mtc.end()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_header() {
        const INVALID_SHORT: &str = "ZCZC-ORG-EEE-+0000-0001122-NOCALL00-";
        const VALID_ONE: &str = "ZCZC-ORG-EEE-012345+0000-0001122-NOCALL00-";
        const VALID_TWO: &str = "ZCZC-ORG-EEE-012345-567890+0000-0001122-NOCALL00-garbage";

        assert_eq!(Err(MessageDecodeErr::TooShort), check_header(INVALID_SHORT));

        assert_eq!(Ok((19, 42)), check_header(VALID_ONE));
        assert_eq!(VALID_ONE.as_bytes()[19], '+' as u8);

        assert_eq!(Ok((26, 49)), check_header(VALID_TWO));
        assert_eq!(VALID_TWO.as_bytes()[26], '+' as u8);
    }

    #[test]
    fn test_message_header() {
        const THREE_LOCATIONS: &str = "ZCZC-ORG-EEE-012345-567890-888990+0351-3661122-NOCALL00-@@@";
        let msg = MessageHeader::try_from((THREE_LOCATIONS.into(), 3)).expect("bad msg");

        assert_eq!(msg.originator(), "ORG");
        assert_eq!(msg.event(), "EEE");
        assert_eq!(msg.valid_duration_str(), "0351");
        assert_eq!(msg.valid_duration(), (3, 51));
        assert_eq!(msg.issue_daytime_str(), "3661122");
        assert_eq!(msg.issue_daytime(), (366, 11, 22));
        assert_eq!(msg.callsign(), "NOCALL00");
        assert_eq!(msg.parity_error_count(), 3);

        let loc: Vec<&str> = msg.location_iter().collect();
        assert_eq!(loc.as_slice(), &["012345", "567890", "888990"]);

        // try again via Message
        let msg = Message::try_from(THREE_LOCATIONS.to_owned()).expect("bad msg");
        match &msg {
            Message::StartOfMessage(m) => assert_eq!(m.issue_daytime(), (366, 11, 22)),
            _ => unreachable!(),
        }
        assert_eq!(&THREE_LOCATIONS[0..56], &format!("{}", msg));
    }

    #[test]
    fn test_message() {
        let msg = Message::try_from("NNNN".to_owned()).expect("bad msg");
        assert_eq!(Message::EndOfMessage, msg);
        assert_eq!("NNNN", &format!("{}", msg));
    }
}
