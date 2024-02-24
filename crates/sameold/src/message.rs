//! SAME message ASCII encoding and decoding

mod eventcode;
mod originator;
mod phenomenon;
mod significance;

use std::convert::TryFrom;
use std::fmt;

#[cfg(feature = "chrono")]
use chrono::{DateTime, Datelike, Duration, NaiveDate, TimeZone, Utc};
use lazy_static::lazy_static;
use regex::Regex;
use thiserror::Error;

pub use eventcode::EventCode;
pub use originator::Originator;
pub use phenomenon::Phenomenon;
pub use significance::SignificanceLevel;

/// The result of parsing a message
pub type MessageResult = Result<Message, MessageDecodeErr>;

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
/// `Message` implements `Display` and efficient conversion to
/// `&str`.
///
/// More information on the SAME/EAS standard may be found in,
/// * "NOAA Weather Radio (NWR) All Hazards Specific Area Message
///   Encoding (SAME)," NWSI 10-172, 3 Oct. 2011,
///   <https://www.nws.noaa.gov/directives/sym/pd01017012curr.pdf>
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Message {
    /// Indicates start of audio message
    ///
    /// A `StartOfMessage` indicates that a SAME/EAS audio
    /// message immediately follows. The message
    /// [header](MessageHeader) contains the event
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
#[derive(Error, Clone, Debug, PartialEq, Eq, Hash)]
pub enum MessageDecodeErr {
    /// The starting prefix of the message was not recognized
    #[error("invalid SAME header: unrecognized prefix")]
    UnrecognizedPrefix,

    /// Header contains non-ASCII characters
    #[error("invalid SAME header: message contains non-ASCII characters")]
    NotAscii,

    /// Header does not match general format
    #[error("invalid SAME header: message text does not match required pattern")]
    Malformed,
}

impl Message {
    /// Convert to string representation
    pub fn as_str(&self) -> &str {
        match self {
            Self::StartOfMessage(m) => m.as_str(),
            Self::EndOfMessage => PREFIX_MESSAGE_END,
        }
    }

    /// Count of parity errors
    ///
    /// The number of *bit errors* which were corrected by the
    /// 2-of-3 parity correction algorithm. High parity error
    /// counts indicate a high bit error rate in the receiving
    /// system.
    ///
    /// Parity errors are *not* tracked for the `EndOfMessage`
    /// variant.
    pub fn parity_error_count(&self) -> usize {
        match self {
            Self::StartOfMessage(m) => m.parity_error_count(),
            Self::EndOfMessage => 0,
        }
    }

    /// Number of bytes which were bit-voted
    ///
    /// `voting_byte_count` is the total number of bytes which were
    /// checked via the "two of three" bitwise voting algorithm—i.e.,
    /// the total number of bytes for which all three SAME bursts were
    /// available.
    ///
    /// Voting counts are *not* tracked for the `EndOfMessage`
    /// variant.
    pub fn voting_byte_count(&self) -> usize {
        match self {
            Self::StartOfMessage(m) => m.voting_byte_count(),
            Self::EndOfMessage => 0,
        }
    }
}

/// An invalid issuance time
#[derive(Error, Clone, Debug, PartialEq, Eq, Hash)]
#[error("message issuance time not valid for its receive time")]
pub struct InvalidDateErr {}

/// Event, area, time, and originator information
///
/// The message header is the decoded *digital header* which precedes
/// the analog SAME message. See
/// [crate documentation](./index.html#interpreting-messages)
/// for an example.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MessageHeader {
    // message content, including the leading `ZCZC-`
    message: String,

    // where does the time field begin?
    // includes the leading plus character (`+`)
    offset_time: usize,

    // parity errors
    parity_error_count: usize,

    // number of message bytes which could be bit-voted
    // (i.e., because three bursts were available)
    voting_byte_count: usize,
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
            voting_byte_count: 0,
        })
    }

    /// Try to construct a SAME header from `String`, with error counts
    ///
    /// The `message` string must match the general format of
    /// a SAME header. If it does not, an error is returned.
    ///
    /// The `error_counts` slice counts the number of bit errors
    /// corrected in byte of `message`. The slice must have the
    /// same length as `message`.
    pub fn new_with_errors<S>(message: S, error_counts: &[u8]) -> Result<Self, MessageDecodeErr>
    where
        S: Into<String>,
    {
        let mut out = Self::new(message)?;
        let mut parity_error_count = 0;
        for (&e, _m) in error_counts.iter().zip(out.message().as_bytes().iter()) {
            parity_error_count += e as usize;
        }

        out.parity_error_count = parity_error_count;
        Ok(out)
    }

    /// Try to construct a SAME header from `String`, with error details
    ///
    /// The `message` string must match the general format of
    /// a SAME header. If it does not, an error is returned.
    ///
    /// The `error_counts` slice counts the number of bit errors
    /// corrected in byte of `message`. The slice must have the
    /// same byte count as `message`.
    ///
    /// `burst_counts` is the total number of SAME bursts which were
    /// used to estimate each message byte. This slice must have
    /// the same byte count as `message`.
    pub fn new_with_error_info<S>(
        message: S,
        error_counts: &[u8],
        burst_counts: &[u8],
    ) -> Result<Self, MessageDecodeErr>
    where
        S: Into<String>,
    {
        const MIN_BURSTS_FOR_VOTING: u8 = 3;

        let mut out = Self::new_with_errors(message, error_counts)?;
        let mut voting_byte_count = 0;
        for (&e, _m) in burst_counts.iter().zip(out.message().as_bytes().iter()) {
            voting_byte_count += (e >= MIN_BURSTS_FOR_VOTING) as usize;
        }
        out.voting_byte_count = voting_byte_count;
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

    /// Message text
    ///
    /// Returns UTF-8 string representation of a SAME/EAS
    /// message. Use the [`release()`](#method.release)
    /// method to obtain an owned `String`.
    pub fn as_str(&self) -> &str {
        &self.message
    }

    /// Originator code
    ///
    /// The ultimate source of the message, such as
    /// [`Originator::NationalWeatherService`] for the
    /// National Weather Service
    pub fn originator(&self) -> Originator {
        Originator::from_org_and_call(self.originator_str(), self.callsign())
    }

    /// Originator code (as string)
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
    /// The originator code returned is three characters but is
    /// not guaranteed to be one of the above.
    pub fn originator_str(&self) -> &str {
        &self.message[Self::OFFSET_ORG..Self::OFFSET_ORG + 3]
    }

    /// Event code
    ///
    /// Decodes the SAME event code (like `RWT`) into an
    /// [`EventCode`], which is a combination of:
    ///
    /// * [`phenomenon()`](Phenomenon), which describes what
    ///   is occurring; and
    ///
    /// * [`significance()`](SignificanceLevel), which indicates the
    ///   overall severity and/or how "noisy" or intrusive the alert
    ///   should be.
    ///
    /// `EventCode` Display as a human-readable string which describes
    /// the SAME code. For example, "`TOR`" displays as "Tornado Warning."
    ///
    /// ```
    /// # use std::fmt;
    /// use sameold::{MessageHeader, Phenomenon, SignificanceLevel};
    ///
    /// let msg = MessageHeader::new("ZCZC-WXR-RWT-012345+0351-3662322-NOCALL  -").unwrap();
    /// let evt = msg.event();
    ///
    /// assert_eq!(evt.phenomenon(), Phenomenon::RequiredWeeklyTest);
    /// assert_eq!(evt.significance(), SignificanceLevel::Test);
    /// assert_eq!(format!("{}", evt), "Required Weekly Test");
    /// ```
    ///
    /// The decoder will make every effort to interpret SAME codes it
    /// does not explicitly know. The `EventCode` might contain only a
    /// valid significance level—or perhaps not even that.
    ///
    /// ```
    /// # use std::fmt;
    /// # use sameold::{MessageHeader, SignificanceLevel};
    /// let msg = MessageHeader::new("ZCZC-WXR-OMG-012345+0351-3662322-NOCALL  -").unwrap();
    /// assert_eq!(msg.event_str(), "OMG");
    /// assert_eq!(msg.event().to_string(), "Unrecognized Warning");
    /// assert_eq!(msg.event().significance(), SignificanceLevel::Unknown);
    /// assert!(msg.event().is_unrecognized());
    /// ```
    ///
    /// Unrecognized messages are still valid, and clients are encouraged
    /// to treat them at their [significance](EventCode::significance) level.
    /// Messages where even the significance level cannot be decoded should
    /// be treated as Warnings.
    ///
    /// [`eventcodes`](crate::eventcodes) contains the complete list of SAME
    /// codes that are interpreted by `sameold`. See also: [`EventCode`].
    pub fn event(&self) -> EventCode {
        EventCode::from(self.event_str())
    }

    /// Event code
    ///
    /// A three-character code like "`RWT`" which describes the phenomenon
    /// and/or the severity level of the message. Use the
    /// [`event()`](MessageHeader::event) method to parse this
    /// code into its components for further processing or for
    /// a human-readable display.
    ///
    /// See [`eventcodes`](crate::eventcodes) for the complete list
    /// of SAME codes that are interpreted by `sameold`. The string value
    /// is not guaranteed to be one of these codes.
    pub fn event_str(&self) -> &str {
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
    pub fn location_str_iter<'m>(&'m self) -> std::str::Split<'m, char> {
        self.location_str().split('-')
    }

    /// Message validity duration (Duration)
    ///
    /// Returns the message validity duration. The message is
    /// valid until
    ///
    /// ```ignore
    /// msg.issue_datetime().unwrap() + msg.valid_duration()
    /// ```
    ///
    /// After this time elapses, the message is no longer valid
    /// and should not be relayed or alerted to anymore.
    ///
    /// This field represents the validity time of the *message*
    /// and not the expected duration of the severe condition.
    /// Severe conditions may persist after the message expires!
    /// (And might be the subject of future messages.)
    ///
    /// The valid duration is relative to the
    /// [`issue_datetime()`](#method.issue_datetime) and *not* the
    /// current time.
    ///
    /// Requires `chrono`.
    #[cfg(feature = "chrono")]
    pub fn valid_duration(&self) -> Duration {
        let (hrs, mins) = self.valid_duration_fields();
        Duration::hours(hrs as i64) + Duration::minutes(mins as i64)
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
    /// [`issue_daytime_fields()`](#method.issue_daytime_fields).
    pub fn valid_duration_fields(&self) -> (u8, u8) {
        let dur_str = &self.message[self.offset_time + Self::OFFSET_FROMPLUS_VALIDTIME
            ..self.offset_time + Self::OFFSET_FROMPLUS_VALIDTIME + 4];
        (
            dur_str[0..2].parse().expect(Self::PANIC_MSG),
            dur_str[2..4].parse().expect(Self::PANIC_MSG),
        )
    }

    /// Estimated message issuance datetime (UTC)
    ///
    /// Computes the datetime that the SAME message was *issued*
    /// from the time that the message was `received`, which
    /// must be provided.
    ///
    /// SAME headers do not include the year of issuance. This makes
    /// it impossible to calculate the full datetime of issuance
    /// without a rough idea of the message's true UTC time. It is
    /// *unnecessary* for the `received` time to be a precision
    /// timestamp. As long as the provided value is within ±90 days
    /// of true UTC, the output time will be correct.
    ///
    /// An error is returned if we are unable to calculate
    /// a valid timestamp. This can happen, for example, if we
    /// project a message sent on Julian/Ordinal Day 366 into a
    /// year that is not a leap year.
    ///
    /// The returned datetime is always in one minute increments
    /// with the seconds field set to zero.
    ///
    /// Requires `chrono`.
    #[cfg(feature = "chrono")]
    pub fn issue_datetime(
        &self,
        received: &DateTime<Utc>,
    ) -> Result<DateTime<Utc>, InvalidDateErr> {
        calculate_issue_time(
            self.issue_daytime_fields(),
            (received.year(), received.ordinal()),
        )
    }

    /// Is the message expired?
    ///
    /// Given the current time, determine if this message has
    /// expired. It is assumed that `now` is within twelve
    /// hours of the message issuance time. Twelve hours is
    /// the maximum [`duration`](#method.valid_duration) of a
    /// SAME message.
    ///
    /// An expired message may still refer to an *ongoing hazard*
    /// or event! Expiration merely indicates that the message
    /// should not be relayed or alerted to anymore.
    ///
    /// Requires `chrono`.
    #[cfg(feature = "chrono")]
    pub fn is_expired_at(&self, now: &DateTime<Utc>) -> bool {
        match self.issue_datetime(now) {
            Ok(issue_ts) => issue_ts + self.valid_duration() < *now,
            Err(_e) => false,
        }
    }

    /// Mesage issuance day/time (fields)
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
    pub fn issue_daytime_fields(&self) -> (u16, u8, u8) {
        let issue = &self.message[self.offset_time + Self::OFFSET_FROMPLUS_ISSUETIME
            ..self.offset_time + Self::OFFSET_FROMPLUS_ISSUETIME + 7];
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
    /// callsign are replaced with slashes (`/`).
    pub fn callsign(&self) -> &str {
        let end = self.message.len();
        &self.message[self.offset_time + Self::OFFSET_FROMPLUS_CALLSIGN
            ..end - Self::OFFSET_FROMEND_CALLSIGN_END]
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

    /// Number of bytes which were bit-voted
    ///
    /// `voting_byte_count` is the total number of bytes which were
    /// checked via the "two of three" bitwise voting algorithm—i.e.,
    /// the total number of bytes for which all three SAME bursts were
    /// available.
    pub fn voting_byte_count(&self) -> usize {
        self.voting_byte_count
    }

    /// True if the message is a national activation
    ///
    /// Returns true if:
    ///
    /// - the location code in the SAME message indicates
    ///   national applicability; and
    ///
    /// - the event code is reserved for national use
    ///
    /// The message may either be a test or an actual emergency.
    /// Consult the [`event()`](MessageHeader::event) for details.
    ///
    /// Clients are **strongly encouraged** to always play
    /// national-level messages and to never provide the option to
    /// suppress them.
    pub fn is_national(&self) -> bool {
        self.location_str() == Self::LOCATION_NATIONAL && self.event().phenomenon().is_national()
    }

    /// Obtain the owned message String
    ///
    /// Destroys this object and releases the message
    /// contained within
    pub fn release(self) -> String {
        self.message
    }

    /// The location portion of the message string
    fn location_str(&self) -> &str {
        &self.message[Self::OFFSET_AREA_START..self.offset_time]
    }

    const OFFSET_ORG: usize = 5;
    const OFFSET_EVT: usize = 9;
    const OFFSET_AREA_START: usize = 13;
    const OFFSET_FROMPLUS_VALIDTIME: usize = 1;
    const OFFSET_FROMPLUS_ISSUETIME: usize = 6;
    const OFFSET_FROMPLUS_CALLSIGN: usize = 14;
    const OFFSET_FROMEND_CALLSIGN_END: usize = 1;
    const PANIC_MSG: &'static str = "MessageHeader validity check admitted a malformed message";
    const LOCATION_NATIONAL: &'static str = "000000";
}

impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
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
        } else if inp.starts_with(&PREFIX_MESSAGE_END[0..2]) {
            Ok(Message::EndOfMessage)
        } else {
            Err(MessageDecodeErr::UnrecognizedPrefix)
        }
    }
}

impl TryFrom<(String, &[u8])> for Message {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: (String, &[u8])) -> Result<Self, Self::Error> {
        if inp.0.starts_with(PREFIX_MESSAGE_START) {
            Ok(Message::StartOfMessage(MessageHeader::try_from(inp)?))
        } else if inp.0.starts_with(&PREFIX_MESSAGE_END[0..2]) {
            Ok(Message::EndOfMessage)
        } else {
            Err(MessageDecodeErr::UnrecognizedPrefix)
        }
    }
}

impl TryFrom<(&[u8], &[u8], &[u8])> for Message {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: (&[u8], &[u8], &[u8])) -> Result<Self, Self::Error> {
        let instr = std::str::from_utf8(inp.0).map_err(|_e| MessageDecodeErr::NotAscii)?;
        if instr.starts_with(PREFIX_MESSAGE_START) {
            Ok(Message::StartOfMessage(MessageHeader::try_from((
                instr.to_owned(),
                inp.1,
                inp.2,
            ))?))
        } else if instr.starts_with(&PREFIX_MESSAGE_END[0..2]) {
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

impl TryFrom<(String, &[u8])> for MessageHeader {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: (String, &[u8])) -> Result<Self, Self::Error> {
        Self::new_with_errors(inp.0, inp.1)
    }
}

impl TryFrom<(String, &[u8], &[u8])> for MessageHeader {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: (String, &[u8], &[u8])) -> Result<Self, Self::Error> {
        Self::new_with_error_info(inp.0, inp.1, inp.2)
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
        static ref RE: Regex = Regex::new(
            r"^ZCZC-[[:alpha:]]{3}-[[:alpha:]]{3}(-[0-9]{6})+(\+[0-9]{4}-[0-9]{7}-.{3,8}-)"
        )
        .expect("bad SAME regexp");
    }

    let mtc = RE
        .captures(hdr)
        .ok_or(MessageDecodeErr::Malformed)?
        .get(2)
        .ok_or(MessageDecodeErr::Malformed)?;

    Ok((mtc.start(), mtc.end()))
}

// Calculate message issuance time
//
// Calculate Utc datetime of message issuance from the
// fields encoded into the `message` and a local estimate
// of when the message was `received`.
#[cfg(feature = "chrono")]
fn calculate_issue_time(
    message: (u16, u8, u8),
    received: (i32, u32),
) -> Result<DateTime<Utc>, InvalidDateErr> {
    let (day_of_year, hour, minute) = message;
    let (rx_year, rx_day_of_year) = received;

    let daydiff = rx_day_of_year as i32 - day_of_year as i32;
    let msg_year = if daydiff >= 180 {
        // message is over 180 days from now, which is unlikely
        // what is more likely is that the UTC new year has
        // arrived and this message is from next year
        rx_year.saturating_add(1)
    } else if daydiff <= -180 {
        // message is over 180 days old, which is unlikely
        // what is more likely is that we have received
        // a message from last UTC year
        rx_year.saturating_sub(1)
    } else {
        // message was received in the current year
        rx_year
    };

    // construct a calendar date
    yo_hms_to_utc(msg_year, day_of_year as u32, hour as u32, minute as u32, 0)
        .ok_or(InvalidDateErr {})
}

// Create the latest-possible Utc date from year, ordinal, and HMS
#[cfg(feature = "chrono")]
#[inline]
fn yo_hms_to_utc(
    year: i32,
    ordinal: u32,
    hour: u32,
    minute: u32,
    second: u32,
) -> Option<DateTime<Utc>> {
    Some(Utc.from_utc_datetime(
        &NaiveDate::from_yo_opt(year, ordinal)?.and_hms_opt(hour, minute, second)?,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "chrono")]
    use chrono::{TimeZone, Utc};

    #[test]
    fn test_check_header() {
        const INVALID_SHORT: &str = "ZCZC-ORG-EEE-+0000-0001122-NOCALL00-";
        const VALID_ONE: &str = "ZCZC-ORG-EEE-012345+0000-0001122-NOCALL00-";
        const VALID_TWO: &str = "ZCZC-ORG-EEE-012345-567890+0000-0001122-NOCALL00-garbage";

        assert_eq!(
            Err(MessageDecodeErr::Malformed),
            check_header(INVALID_SHORT)
        );

        assert_eq!(Ok((19, 42)), check_header(VALID_ONE));
        assert_eq!(VALID_ONE.as_bytes()[19], '+' as u8);

        assert_eq!(Ok((26, 49)), check_header(VALID_TWO));
        assert_eq!(VALID_TWO.as_bytes()[26], '+' as u8);
    }

    #[test]
    #[cfg(feature = "chrono")]
    fn test_calculate_issue_time() {
        let d = calculate_issue_time((83, 2, 53), (2021, 1)).unwrap();
        assert_eq!(d, Utc.with_ymd_and_hms(2021, 3, 24, 2, 53, 0).unwrap());

        let d = calculate_issue_time((84, 23, 59), (2021, 1)).unwrap();
        assert_eq!(d, Utc.with_ymd_and_hms(2021, 3, 25, 23, 59, 0).unwrap());

        // close to the current year
        let d = calculate_issue_time((1, 10, 00), (2021, 1)).unwrap();
        assert_eq!(d, Utc.with_ymd_and_hms(2021, 1, 1, 10, 00, 0).unwrap());

        // bumps to next year
        let d = calculate_issue_time((1, 10, 00), (2021, 200)).unwrap();
        assert_eq!(d, Utc.with_ymd_and_hms(2022, 1, 1, 10, 00, 0).unwrap());

        // this too
        let d = calculate_issue_time((1, 10, 00), (2021, 365)).unwrap();
        assert_eq!(d, Utc.with_ymd_and_hms(2022, 1, 1, 10, 00, 0).unwrap());

        // reverts to previous year, with leap year support
        let d = calculate_issue_time((366, 10, 00), (2021, 1)).unwrap();
        assert_eq!(d, Utc.with_ymd_and_hms(2020, 12, 31, 10, 00, 0).unwrap());

        // but this doesn't work at all if the year we propagate into
        // is not a leap year
        calculate_issue_time((366, 10, 00), (1971, 364)).expect_err("should not succeed");

        // and ordinal day 0 is totally invalid
        calculate_issue_time((0, 10, 00), (1971, 364)).expect_err("should not succeed");

        // hours invalid
        calculate_issue_time((84, 25, 59), (2021, 84)).expect_err("should not succeed");
    }

    #[test]
    fn test_message_header() {
        const THREE_LOCATIONS: &str = "ZCZC-WXR-RWT-012345-567890-888990+0351-3662322-NOCALL00-@@@";

        let mut errs = vec![0u8; THREE_LOCATIONS.len()];
        errs[0] = 1u8;
        errs[20] = 5u8;
        errs[THREE_LOCATIONS.len() - 1] = 8u8;

        let burst_count = vec![3u8; THREE_LOCATIONS.len()];

        let msg = MessageHeader::try_from((
            THREE_LOCATIONS.to_owned(),
            errs.as_slice(),
            burst_count.as_slice(),
        ))
        .expect("bad msg");

        assert_eq!(msg.originator_str(), "WXR");
        assert_eq!(Originator::NationalWeatherService, msg.originator());
        assert_eq!(msg.event_str(), "RWT");
        assert_eq!(msg.event().phenomenon(), Phenomenon::RequiredWeeklyTest);
        assert_eq!(msg.valid_duration_fields(), (3, 51));
        assert_eq!(msg.issue_daytime_fields(), (366, 23, 22));
        assert_eq!(msg.callsign(), "NOCALL00");
        assert_eq!(msg.parity_error_count(), 6);
        assert_eq!(msg.voting_byte_count(), msg.as_str().len());
        assert!(!msg.is_national());

        let loc: Vec<&str> = msg.location_str_iter().collect();
        assert_eq!(loc.as_slice(), &["012345", "567890", "888990"]);

        // time API checks
        #[cfg(feature = "chrono")]
        {
            assert_eq!(
                Utc.with_ymd_and_hms(2020, 12, 31, 23, 22, 00).unwrap(),
                msg.issue_datetime(&Utc.with_ymd_and_hms(2020, 12, 31, 11, 30, 34).unwrap())
                    .unwrap()
            );
            assert_eq!(
                msg.valid_duration(),
                Duration::hours(3) + Duration::minutes(51)
            );
            assert!(!msg.is_expired_at(&Utc.with_ymd_and_hms(2020, 12, 31, 23, 59, 0).unwrap()));
            assert!(!msg.is_expired_at(&Utc.with_ymd_and_hms(2021, 1, 1, 1, 20, 30).unwrap()));
            assert!(!msg.is_expired_at(&Utc.with_ymd_and_hms(2021, 1, 1, 3, 13, 00).unwrap()));
            assert!(msg.is_expired_at(&Utc.with_ymd_and_hms(2021, 1, 1, 3, 13, 01).unwrap()));
        }

        // try again via Message
        let msg = Message::try_from(THREE_LOCATIONS.to_owned()).expect("bad msg");
        match &msg {
            Message::StartOfMessage(m) => assert_eq!(m.issue_daytime_fields(), (366, 23, 22)),
            _ => unreachable!(),
        }
        assert_eq!(&THREE_LOCATIONS[0..56], &format!("{}", msg));
    }

    #[test]
    fn test_message() {
        let msg = Message::try_from("NNNN".to_owned()).expect("bad msg");
        assert_eq!(Message::EndOfMessage, msg);
        assert_eq!("NNNN", &format!("{}", msg));

        let msg = Message::try_from("NN".to_owned()).expect("bad msg");
        assert_eq!(Message::EndOfMessage, msg);
    }

    #[test]
    fn test_is_national() {
        let national = MessageHeader::new("ZCZC-PEP-NPT-000000+0030-2771820-TEST    -").unwrap();
        assert!(national.is_national());

        let national = MessageHeader::new("ZCZC-PEP-EAN-000000+0030-2771820-TEST    -").unwrap();
        assert!(national.is_national());

        let not_national =
            MessageHeader::new("ZCZC-PEP-NPT-000001+0030-2771820-TEST    -").unwrap();
        assert!(!not_national.is_national());

        let not_national =
            MessageHeader::new("ZCZC-PEP-NPT-000000-000001+0030-2771820-TEST    -").unwrap();
        assert!(!not_national.is_national());
    }
}
