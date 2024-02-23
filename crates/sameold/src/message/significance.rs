//! Significance level

use std::fmt;

use strum::EnumMessage;

/// SAME message significance level
///
/// Usually constructed as part of an [`EventCode`](crate::EventCode).
/// See also [`MessageHeader::event()`](crate::MessageHeader::event)
///
/// Three-letter SAME codes sometimes use the last letter to
/// indicate *significance* or severity.
///
/// | Code    | Significance                                      |
/// |---------|---------------------------------------------------|
/// | `xxT`   | [test](crate::SignificanceLevel::Test)            |
/// | `xxM`   | [message](crate::SignificanceLevel::Message)      |
/// | `xxS`   | [statement](crate::SignificanceLevel::Statement)  |
/// | `xxE`   | [emergency](crate::SignificanceLevel::Emergency)  |
/// | `xxA`   | [watch](crate::SignificanceLevel::Watch)          |
/// | `xxW`   | [warning](crate::SignificanceLevel::Warning)      |
///
/// There are many message codes which do not follow this standard—and
/// some even contradict it. sameold knows the correct significance
/// code for these special cases, and the
/// [event](crate::MessageHeader::event) API will return it.
///
/// Significance codes can be converted directly from or to string.
///
/// ```
/// use sameold::SignificanceLevel;
///
/// assert_eq!(SignificanceLevel::Watch, SignificanceLevel::from("A"));
/// assert_eq!(SignificanceLevel::Test, SignificanceLevel::from("T"));
///
/// assert_eq!("Test", SignificanceLevel::Test.as_display_str());
/// assert_eq!("Test", format!("{}", SignificanceLevel::Test));
/// assert_eq!("T", SignificanceLevel::Test.as_code_str());
/// assert_eq!("T", format!("{:#}", SignificanceLevel::Test));
/// ```
///
/// Significance levels are `Ord`. Lower significance levels
/// represent less urgent messages, such as tests and statements.
/// Higher significance levels represent more important or urgent
/// messages which may merit a "noisy" notification.
///
/// ```
/// # use sameold::SignificanceLevel;
/// assert!(SignificanceLevel::Test < SignificanceLevel::Warning);
/// assert!(SignificanceLevel::Watch < SignificanceLevel::Warning);
/// ```
///
/// Unrecognized significance levels are quietly represented as
/// [`SignificanceLevel::Unknown`]. Clients are encouraged to treat
/// messages with this significance level as a Warning.
///
/// ```
/// # use sameold::SignificanceLevel;
/// assert_eq!(SignificanceLevel::Unknown, SignificanceLevel::from(""));
/// assert!(SignificanceLevel::Unknown >= SignificanceLevel::Warning);
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

    /// Unknown significance level
    ///
    /// No significance level could be determined, either by knowledge of
    /// the complete event code or by examining the last character.
    /// Clients are strongly advised to treat unknown-significance messages
    /// as [`SignificanceLevel::Warning`].
    #[strum(serialize = "", detailed_message = "Warning")]
    Unknown,
}

impl SignificanceLevel {
    /// Parse from string
    ///
    /// Parses a SAME significance level from a single-character
    /// `code` like "`T`" for [`SignificanceLevel::Test`]. If the
    /// input does not match a significance level, returns
    /// [`SignificanceLevel::Unknown`].
    ///
    /// The user is cautioned not to blindly convert the last
    /// character of a SAME code to a `SignificanceLevel`. There
    /// are many event codes like "`EVI`" which do not follow the
    /// `SignificanceLevel` convention.
    pub fn from<S>(code: S) -> Self
    where
        S: AsRef<str>,
    {
        str::parse(code.as_ref()).unwrap_or_default()
    }

    /// Human-readable string representation
    ///
    /// Converts to a human-readable string, like "`Warning`."
    pub fn as_display_str(&self) -> &'static str {
        self.get_detailed_message().expect("missing definition")
    }

    /// SAME string representation
    ///
    /// Returns the one-character SAME code for this
    /// `SignificanceLevel`. While this is *frequently* the last
    /// character of the event code, there are almost as many
    /// exceptions to this rule as there are codes which
    /// follow it.
    pub fn as_code_str(&self) -> &'static str {
        self.get_serializations()[0]
    }
}

impl std::default::Default for SignificanceLevel {
    fn default() -> Self {
        SignificanceLevel::Unknown
    }
}

impl From<&str> for SignificanceLevel {
    fn from(s: &str) -> SignificanceLevel {
        SignificanceLevel::from(s)
    }
}

impl AsRef<str> for SignificanceLevel {
    fn as_ref(&self) -> &'static str {
        self.as_code_str()
    }
}

impl fmt::Display for SignificanceLevel {
    /// Printable string
    ///
    /// * The normal form is a human-readable string like "`Statement`"
    /// * The alternate form is a one-character string like "`S`"
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.as_code_str().fmt(f)
        } else {
            self.as_display_str().fmt(f)
        }
    }
}
