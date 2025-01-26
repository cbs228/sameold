//! Event decoding and representation

use std::fmt;

use crate::eventcodes::{parse_event, CodeEntry};

use super::phenomenon::Phenomenon;
use super::significance::SignificanceLevel;

/// Decoded SAME event code
///
/// Represents the decoding of a three-character SAME event code,
/// like "`RWT`," into a [phenomenon](EventCode::phenomenon) and
/// [significance](EventCode::significance).
///
/// * The phenomenon describes what is occurring
///
/// * The significance indicates the overall severity and/or how
///   "noisy" or intrusive the alert should be.
///
/// EventCode are usually constructed via
/// [`MessageHeader::event()`](crate::MessageHeader::event) but may also
/// be directly created from string.
///
/// ```
/// use sameplace::{EventCode, Phenomenon, SignificanceLevel};
///
/// let evt = EventCode::from("RWT");
/// assert_eq!(evt.phenomenon(), Phenomenon::RequiredWeeklyTest);
/// assert_eq!(evt.significance(), SignificanceLevel::Test);
/// ```
///
/// EventCode are `Ord` by their significance levels.
///
/// ```
/// # use sameplace::EventCode;
/// assert!(EventCode::from("RWT") < EventCode::from("SVA"));
/// assert!(EventCode::from("SVA") < EventCode::from("SVR"));
/// ```
///
/// The `Display` representation is a human-readable string representing
/// both phenomenon and significance.
///
/// ```
/// # use sameplace::EventCode;
/// # use std::fmt;
/// assert_eq!(EventCode::from("SVA").to_string(), "Severe Thunderstorm Watch");
/// ```
///
/// The conversion from string is infallible, but invalid strings will
/// result in an [unrecognized](EventCode::is_unrecognized) message.
///
/// ```
/// # use sameplace::{EventCode, SignificanceLevel};
/// let watch = EventCode::from("??A");
/// assert!(watch.is_unrecognized());
/// assert_eq!(watch.significance(), SignificanceLevel::Watch);
/// assert_eq!(watch.to_string(), "Unrecognized Watch");
///
/// let unrec = EventCode::from("???");
/// assert!(unrec.is_unrecognized());
/// assert_eq!(unrec.significance(), SignificanceLevel::Unknown);
/// assert_eq!(unrec.to_string(), "Unrecognized Warning");
/// ```
///
/// If the phenomenon portion cannot be decoded, the third character
/// is parsed as a `SignificanceLevel` if possible. Unrecognized messages
/// are still valid, and clients are encouraged to handle them at their
/// [significance level](EventCode::significance) as normal.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct EventCode {
    phenomenon: Phenomenon,
    significance: SignificanceLevel,
}

impl EventCode {
    /// Parse from SAME code, like "`RWT`"
    ///
    /// This type is usually constructed via
    /// [`MessageHeader::event()`](crate::MessageHeader::event), but
    /// you can also construct them directly. This method decodes the
    /// string representation of a three-character SAME event `code`,
    /// like "`RWT`," into a machine-readable event.
    ///
    /// If the input `code` is not known to `sameplace`, is not in the
    /// required format (i.e., three ASCII characters), or is otherwise
    /// not valid, the output of
    /// [`is_unrecognized()`](EventCode::is_unrecognized) will be
    /// `true`.
    pub fn from<S>(code: S) -> Self
    where
        S: AsRef<str>,
    {
        parse_event(code).unwrap_or_default().into()
    }

    /// What is occurring?
    pub fn phenomenon(&self) -> Phenomenon {
        self.phenomenon
    }

    /// What is the anticipated severity?
    pub fn significance(&self) -> SignificanceLevel {
        self.significance
    }

    /// Human-readable string representation
    ///
    /// Converts to a human-readable string, like "`Required Monthly Test`."
    pub fn to_display_string(&self) -> String {
        self.to_string()
    }

    /// True for test messages
    ///
    /// Test messages do not represent real-life events or emergencies.
    pub fn is_test(&self) -> bool {
        self.significance() == SignificanceLevel::Test || self.phenomenon().is_test()
    }

    /// True if any part of the event code was unrecognized
    ///
    /// Indicates that either the phenomenon or the significance
    /// could not be determined from the input SAME code.
    ///
    /// Unrecognized messages are still valid, and clients are
    /// encouraged to handle them at their
    /// [significance level](EventCode::significance) as normal.
    pub fn is_unrecognized(&self) -> bool {
        self.phenomenon == Phenomenon::Unrecognized
            || self.significance == SignificanceLevel::Unknown
    }

    /// An unrecognized event code
    pub(crate) const fn unrecognized() -> Self {
        Self {
            phenomenon: Phenomenon::Unrecognized,
            significance: SignificanceLevel::Unknown,
        }
    }
}

impl Default for EventCode {
    fn default() -> Self {
        Self::unrecognized()
    }
}

impl From<&str> for EventCode {
    fn from(value: &str) -> Self {
        EventCode::from(value)
    }
}

impl From<&String> for EventCode {
    fn from(value: &String) -> Self {
        EventCode::from(value)
    }
}

impl fmt::Display for EventCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.phenomenon().fmt(f)
        } else {
            let phenom_pattern = self.phenomenon().as_full_pattern_str();
            if let Some(phenom_need_sig) = phenom_pattern.strip_suffix("%") {
                // pattern string needs significance
                write!(f, "{}{}", phenom_need_sig, self.significance)
            } else {
                // pattern string is complete
                phenom_pattern.fmt(f)
            }
        }
    }
}

impl Ord for EventCode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.significance().cmp(&other.significance())
    }
}

impl PartialOrd for EventCode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<CodeEntry> for EventCode {
    fn from(value: CodeEntry) -> Self {
        Self {
            phenomenon: value.0,
            significance: value.1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unrecognized() {
        assert_eq!(EventCode::from(""), EventCode::default());
        assert_eq!(EventCode::default(), EventCode::unrecognized());
    }

    #[test]
    fn basic_parsing() {
        let unk = EventCode::from("");
        assert_eq!(unk, EventCode::default());
        assert_eq!(Phenomenon::Unrecognized, unk.phenomenon());
        assert_eq!(SignificanceLevel::Unknown, unk.significance());

        let code_tor = EventCode::from("TOR");
        assert_eq!(Phenomenon::Tornado, code_tor.phenomenon());
        assert_eq!(SignificanceLevel::Warning, code_tor.significance());

        let code_toe = EventCode::from("TOE");
        assert_eq!(Phenomenon::TelephoneOutage, code_toe.phenomenon());
        assert_eq!(SignificanceLevel::Emergency, code_toe.significance());

        let code_toa = EventCode::from("TOA");
        assert_eq!(Phenomenon::Tornado, code_toa.phenomenon());
        assert_eq!(SignificanceLevel::Watch, code_toa.significance());

        // this is **NOT** a valid SAME code… but since the `TO`
        // prefix is also used for two-character decoding. this still
        // works
        let code_tow = EventCode::from("TOW");
        assert_eq!(Phenomenon::Tornado, code_tow.phenomenon());
        assert_eq!(SignificanceLevel::Warning, code_tow.significance());

        // four-character codes do not fly
        assert_eq!(EventCode::from("TORZ"), EventCode::default());

        // this code is unknown, but we can still extract a significance
        let code_dew = EventCode::from("DEW");
        assert_eq!(Phenomenon::Unrecognized, code_dew.phenomenon());
        assert_eq!(SignificanceLevel::Warning, code_dew.significance());

        // an unknown significance on a two-character code
        let code_bz = EventCode::from("BZ!");
        assert_eq!(Phenomenon::Blizzard, code_bz.phenomenon());
        assert_eq!(SignificanceLevel::Unknown, code_bz.significance());
    }

    #[test]
    fn basic_display() {
        let evt = EventCode::from("EAN");
        assert_eq!("National Emergency Message", evt.to_string());

        // three-character codes sometimes bake in their significance
        let evt = EventCode::from("TOR");
        assert_eq!("Tornado Warning", evt.to_string());

        // two-character codes must populate
        let evt = EventCode::from("BZW");
        assert_eq!("Blizzard Warning", evt.to_string());

        // not really SAME, but it decodes.
        let evt = EventCode::from("BZS");
        assert_eq!("Blizzard Statement", evt.to_string());

        // alternate format describes event w/o significance
        let evt = EventCode::from("TOE");
        assert_eq!("911 Telephone Outage", &format!("{:#}", evt));
        assert_eq!("911 Telephone Outage Emergency", &format!("{}", evt));

        let evt = EventCode::from("EVI");
        assert_eq!("Evacuation", &format!("{:#}", evt));
        assert_eq!("Evacuation Immediate", &format!("{}", evt));

        // we still have a display for completely unknown codes
        let evt = EventCode::from("!!!");
        assert_eq!("Unrecognized Warning", &format!("{}", evt));
        assert_eq!("Unrecognized", &format!("{:#}", evt));
    }

    #[test]
    fn test_support_required_codes() {
        // Event codes from crate::eventcodes docstring
        const TEST_CODES: &[&str] = &[
            "ADR", "AVA", "AVW", "BLU", "BZW", "CAE", "CDW", "CEM", "CFA", "CFW", "DMO", "DSW",
            "EAN", "EQW", "EVI", "EWW", "FFA", "FFS", "FFW", "FLA", "FLS", "FLW", "FRW", "FSW",
            "FZW", "HLS", "HMW", "HUA", "HUW", "HWA", "HWW", "LAE", "LEW", "NAT", "NIC", "NMN",
            "NPT", "NST", "NUW", "RHW", "RMT", "RWT", "SMW", "SPS", "SPW", "SQW", "SSA", "SSW",
            "SVA", "SVR", "SVS", "TOA", "TOE", "TOR", "TRA", "TRW", "TSA", "TSW", "VOW", "WSA",
            "WSW",
        ];

        for code in TEST_CODES.iter().cloned() {
            let evt = EventCode::from(code);
            assert!(
                evt.phenomenon().is_recognized(),
                "event code {} was not recognized",
                code
            );

            assert!(
                evt.significance() != SignificanceLevel::Unknown,
                "event code {} has unknown significance; must be known",
                code
            );

            // ensure display does not contain format code
            let disp = format!("{}", evt);
            assert!(!disp.contains("%"));

            // all test messages should have a significance of test
            if evt.phenomenon().is_test() {
                assert_eq!(evt.significance(), SignificanceLevel::Test);
            }
        }
    }
}
