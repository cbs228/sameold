//! Framer outputs for the client

use std::convert::TryFrom;
use std::fmt;

#[cfg(feature = "chrono")]
use chrono::{DateTime, Datelike, Duration, TimeZone, Utc};
use lazy_static::lazy_static;
use regex::Regex;
use thiserror::Error;

use crate::samecodes::{EventCode, Originator, UnrecognizedEventCode};

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

    /// Header is shorter than the minimum length for a valid message
    #[error("invalid SAME header: decoded message too short")]
    TooShort,

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
}

/// An invalid issuance time
#[derive(Error, Clone, Debug, PartialEq, Eq, Hash)]
#[error("message issuance time not valid for its receive time")]
pub struct InvalidDateErr {}

/// Event, area, time, and originator information
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    /// `Originator::WeatherService` for the National Weather Service
    pub fn originator(&self) -> Originator {
        Originator::from((self.originator_str(), self.callsign()))
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
    /// Decodes the event code into an enumerated type.
    /// For example, messages which contain an
    /// [`event_str()`](#method.event_str) of "`RWT`" will decode
    /// as [`EventCode::RequiredWeeklyTest`](EventCode#variant.RequiredWeeklyTest).
    ///
    /// If the event code is unrecognized, an error is returned.
    /// An error here does **NOT** mean that the message is
    /// invalid or should be discarded. Instead, if the
    /// error is
    /// [`WithSignificance`](UnrecognizedEventCode#variant.WithSignificance),
    /// then you should treat it as a valid (but unknown)
    /// message at the given significance level. This will help
    /// your application react correctly if new codes are
    /// added in the future.
    ///
    /// Event codes can be converted to human-readable strings.
    ///
    /// ```
    /// use sameold::EventCode;
    ///
    /// assert_eq!("Required Weekly Test", (EventCode::RequiredWeeklyTest).as_display_str());
    /// assert_eq!(
    ///     "Required Weekly Test",
    ///     format!("{}", EventCode::RequiredWeeklyTest)
    /// );
    /// ```
    ///
    /// All `EventCode` are mapped to a [significance level](crate::SignificanceLevel).
    /// This may be useful when deciding how to handle the event.
    ///
    /// ```
    /// # use sameold::{EventCode, SignificanceLevel};
    ///
    /// let lvl = (EventCode::RequiredWeeklyTest).to_significance_level();
    /// assert_eq!(lvl, SignificanceLevel::Test);
    /// ```
    pub fn event(&self) -> Result<EventCode, UnrecognizedEventCode> {
        EventCode::try_from(self.event_str())
    }

    /// Event code
    ///
    /// A three-character code which is *generally* formatted
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
    /// messages in the United States. Plenty of other codes
    /// also do not adhere to this standard.
    ///
    /// The event code returned is three characters but is
    /// not guaranteed to be one of the above.
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
        let locations = &self.message[Self::OFFSET_AREA_START..self.offset_time];
        locations.split('-')
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
    const OFFSET_FROMPLUS_CALLSIGN: usize = 14;
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

impl TryFrom<(String, &[u8])> for Message {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: (String, &[u8])) -> Result<Self, Self::Error> {
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

impl TryFrom<(String, &[u8])> for MessageHeader {
    type Error = MessageDecodeErr;

    #[inline]
    fn try_from(inp: (String, &[u8])) -> Result<Self, Self::Error> {
        Self::new_with_errors(inp.0, inp.1)
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
            Regex::new(r"^ZCZC-[A-Z]{3}-[A-Z]{3}(-[0-9]{6})+(\+[0-9]{4}-[0-9]{7}-.{3,8}-)")
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
    Ok(Utc
        .yo_opt(msg_year, day_of_year as u32)
        .single()
        .ok_or(InvalidDateErr {})?
        .and_hms_opt(hour as u32, minute as u32, 0)
        .ok_or(InvalidDateErr {})?)
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
    #[cfg(feature = "chrono")]
    fn test_calculate_issue_time() {
        let d = calculate_issue_time((83, 2, 53), (2021, 1)).unwrap();
        assert_eq!(d, Utc.ymd(2021, 3, 24).and_hms(2, 53, 0));

        let d = calculate_issue_time((84, 23, 59), (2021, 1)).unwrap();
        assert_eq!(d, Utc.ymd(2021, 3, 25).and_hms(23, 59, 0));

        // close to the current year
        let d = calculate_issue_time((1, 10, 00), (2021, 1)).unwrap();
        assert_eq!(d, Utc.ymd(2021, 1, 1).and_hms(10, 00, 0));

        // bumps to next year
        let d = calculate_issue_time((1, 10, 00), (2021, 200)).unwrap();
        assert_eq!(d, Utc.ymd(2022, 1, 1).and_hms(10, 00, 0));

        // this too
        let d = calculate_issue_time((1, 10, 00), (2021, 365)).unwrap();
        assert_eq!(d, Utc.ymd(2022, 1, 1).and_hms(10, 00, 0));

        // reverts to previous year, with leap year support
        let d = calculate_issue_time((366, 10, 00), (2021, 1)).unwrap();
        assert_eq!(d, Utc.ymd(2020, 12, 31).and_hms(10, 00, 0));

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

        let msg = MessageHeader::try_from((THREE_LOCATIONS.to_owned(), errs.as_slice()))
            .expect("bad msg");

        assert_eq!(msg.originator_str(), "WXR");
        assert_eq!(Originator::WeatherService, msg.originator());
        assert_eq!(msg.event_str(), "RWT");
        assert_eq!(msg.event().unwrap(), EventCode::RequiredWeeklyTest);
        assert_eq!(msg.valid_duration_fields(), (3, 51));
        assert_eq!(msg.issue_daytime_fields(), (366, 23, 22));
        assert_eq!(msg.callsign(), "NOCALL00");
        assert_eq!(msg.parity_error_count(), 6);

        let loc: Vec<&str> = msg.location_str_iter().collect();
        assert_eq!(loc.as_slice(), &["012345", "567890", "888990"]);

        // time API checks
        #[cfg(feature = "chrono")]
        {
            assert_eq!(
                Utc.ymd(2020, 12, 31).and_hms(23, 22, 00),
                msg.issue_datetime(&Utc.ymd(2020, 12, 31).and_hms(11, 30, 34))
                    .unwrap()
            );
            assert_eq!(
                msg.valid_duration(),
                Duration::hours(3) + Duration::minutes(51)
            );
            assert!(!msg.is_expired_at(&Utc.ymd(2020, 12, 31).and_hms(23, 59, 0)));
            assert!(!msg.is_expired_at(&Utc.ymd(2021, 1, 1).and_hms(1, 20, 30)));
            assert!(!msg.is_expired_at(&Utc.ymd(2021, 1, 1).and_hms(3, 13, 00)));
            assert!(msg.is_expired_at(&Utc.ymd(2021, 1, 1).and_hms(3, 13, 01)));
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
    }
}
