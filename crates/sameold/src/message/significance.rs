//! Significance level

use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;

use strum::EnumMessage;
use thiserror::Error;

/// SAME message significance level
///
/// Usually constructed as part of an [`EventCode`].
/// See also [MessageHeader::event()](crate::MessageHeader#method.event)
///
/// Significance levels have a single-character text
/// representation, like "`T`" for Test. You can attempt to
/// convert from string:
///
/// ```
/// # use std::convert::TryFrom;
/// use sameold::{SignificanceLevel, UnknownSignificanceLevel};
///
/// assert_eq!(SignificanceLevel::Watch, SignificanceLevel::try_from("zzA").unwrap());
/// assert_eq!(UnknownSignificanceLevel {}, SignificanceLevel::try_from("").unwrap_err());
/// assert_eq!(SignificanceLevel::Test, SignificanceLevel::try_from("T").unwrap());
/// ```
///
/// If a multi-character string is given as input, the last character
/// will be used.  The last byte must be valid UTF-8. In all situations
/// where a valid `SignificanceLevel` can't be constructed, an
/// error is returned.
///
/// Significance levels are `Ord`. Lower significance levels
/// represent less urgent messages, such as tests and statements.
/// Higher significance levels represent more important or urgent
/// messages which may merit a "noisy" notification.
///
/// ```
/// # use sameold::SignificanceLevel;
///
/// assert!(SignificanceLevel::Test < SignificanceLevel::Warning);
/// assert!(SignificanceLevel::Watch < SignificanceLevel::Warning);
/// ```
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    strum_macros::EnumMessage,
    strum_macros::EnumString,
)]
#[repr(u8)]
pub enum SignificanceLevel {
    /// Test
    ///
    /// A message intended only for testing purposes. "This is only a test."
    #[strum(serialize = "T", detailed_message = "Test")]
    Test,

    /// Message
    ///
    /// A non-emergency message
    #[strum(serialize = "M", detailed_message = "Message")]
    Message,

    /// Statement
    ///
    /// > A message containing follow up information to a warning, watch,
    /// > or emergency (NWSI 10-1712).
    #[strum(serialize = "S", detailed_message = "Statement")]
    Statement,

    /// Emergency
    ///
    /// > An event that by itself would not kill or injure or do property
    /// > damage, but indirectly may cause other things to happen that
    /// > result in a hazard. Example, a major power or telephone loss in
    /// > a large city alone is not a direct hazard but disruption to
    /// > other critical services could create a variety of conditions
    /// > that could directly threaten public safety (NWSI 10-1712).
    #[strum(serialize = "E", detailed_message = "Emergency")]
    Emergency,

    /// Watch
    ///
    /// > Meets the classification of a warning, but either the onset time,
    /// > probability of occurrence, or location is uncertain (NWSI 10-1712).
    #[strum(serialize = "A", detailed_message = "Watch")]
    Watch,

    /// Warning (the most severe event)
    ///
    /// > Those events that alone pose a significant threat to public
    /// > safety and/or property, probability of occurrence and location
    /// > is high, and the onset time is relatively short (NWSI 10-1712).
    #[strum(serialize = "W", detailed_message = "Warning")]
    Warning,
}

impl SignificanceLevel {
    /// Human-readable string representation
    ///
    /// Converts to a human-readable string, like "`Warning`."
    pub fn as_display_str(&self) -> &'static str {
        self.get_detailed_message().expect("missing definition")
    }

    /// SAME string representation
    ///
    /// Returns the one-character SAME code for this
    /// `SignificanceLevel`. While this is *usually* the last
    /// character of the `EventCode`, there are many exceptions
    /// to this rule.
    pub fn as_str(&self) -> &'static str {
        self.get_serializations()[0]
    }
}

impl AsRef<str> for SignificanceLevel {
    fn as_ref(&self) -> &'static str {
        self.as_str()
    }
}

impl fmt::Display for SignificanceLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_display_str().fmt(f)
    }
}

impl TryFrom<&str> for SignificanceLevel {
    type Error = UnknownSignificanceLevel;

    /// Convert from string representation
    ///
    /// Matches a standard-form EAS event code, such as `xxT` for
    /// Test, and converts it to its enumerated type. If the given
    /// string does not end in a significance level,
    /// `UnknownSignificanceLevel` is returned.
    fn try_from(inp: &str) -> Result<Self, Self::Error> {
        let s = last_ascii_character(inp).ok_or(UnknownSignificanceLevel {})?;
        SignificanceLevel::from_str(s).map_err(|_| UnknownSignificanceLevel {})
    }
}

/// Unknown significance level
///
/// The event code is not known, and we were unable to determine
/// a significance level from it.
#[derive(Error, Clone, Debug, PartialEq, Eq)]
#[error("The event significance level could not be determined")]
pub struct UnknownSignificanceLevel {}

// Get last character of the given ASCII string
//
// The last byte of `s` must be valid UTF-8. Returns reference
// to the last byte.
fn last_ascii_character<'a>(s: &'a str) -> Option<&'a str> {
    if s.is_empty() {
        None
    } else {
        s.get((s.len() - 1)..s.len())
    }
}
