//! SAME/EAS Originator and Event Codes

use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;

use strum::{EnumMessage, EnumProperty};
use thiserror::Error;

/// SAME message originator code
///
/// See [Message::originator()](struct.Message.html#method.originator).
/// Originator codes may be converted `from()` their SAME string
/// representations. Using them `.as_ref()` or via `Display` will
/// show a human-readable string.
///
/// ```
/// use sameold::Originator;
///
/// let orig = Originator::from("WXR");
/// assert_eq!(Originator::WeatherService, orig);
/// assert_eq!("WXR", orig.as_ref());
/// assert_eq!("National Weather Service", orig.as_display_str());
/// assert_eq!("National Weather Service", &format!("{}", orig));
///
/// assert_eq!(Originator::Unknown, Originator::from("HUH"));
/// ```
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, Hash, strum_macros::EnumMessage, strum_macros::EnumString,
)]
pub enum Originator {
    /// An unknown (and probably invalid) Originator code
    ///
    /// Per NWSI 10-172, receivers should accept any originator code.
    #[strum(serialize = "OOO", detailed_message = "Unknown Originator")]
    Unknown,

    /// Primary Entry Point station for national activations
    ///
    /// Nation-wide activations are authorized by the President of
    /// the United States. Takes priority over all other
    /// messages/station programming.
    #[strum(serialize = "PEP", detailed_message = "Primary Entry Point System")]
    PrimaryEntryPoint,

    /// Civil authorities
    #[strum(serialize = "CIV", detailed_message = "Civil authorities")]
    CivilAuthority,

    /// National Weather Service or Environment Canada
    #[strum(serialize = "WXR", detailed_message = "National Weather Service")]
    WeatherService,

    /// EAS participant (usu. broadcast station)
    #[strum(
        serialize = "EAS",
        detailed_message = "Broadcast station or cable system"
    )]
    BroadcastStation,
}

impl Originator {
    /// Human-readable string representation
    ///
    /// Converts to a human-readable string, like "`Civil authorities`."
    pub fn as_display_str(&self) -> &'static str {
        self.get_detailed_message().expect("missing definition")
    }

    /// SAME string representation
    ///
    /// Returns the three-character SAME code for this
    /// `Originator`
    pub fn as_str(&self) -> &'static str {
        self.get_serializations()[0]
    }
}

impl From<&str> for Originator {
    fn from(s: &str) -> Originator {
        match Originator::from_str(s) {
            Ok(orig) => orig,
            Err(_e) => Originator::Unknown,
        }
    }
}

impl AsRef<str> for Originator {
    fn as_ref(&self) -> &'static str {
        self.as_str()
    }
}

impl fmt::Display for Originator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_display_str().fmt(f)
    }
}

/// SAME message significance level
///
/// Usually constructed as part of an [`EventCode`](enum.EventCode.html).
/// See also [MessageHeader::event()](struct.MessageHeader.html#method.event)
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

#[derive(Error, Clone, Debug, PartialEq, Eq)]
#[error("The event significance level could not be determined")]
pub struct UnknownSignificanceLevel {}

/// SAME message event code
///
/// Usually constructed via
/// [MessageHeader::event()](struct.MessageHeader.html#method.event).
/// Event codes were obtained from
/// <https://docs.fcc.gov/public/attachments/FCC-16-80A1.pdf>.
///
/// Converting to string via `.as_ref()` will yield the SAME
/// event code string. You can also obtain a human-readable message
///
/// ```
/// use sameold::EventCode;
///
/// assert_eq!("RWT", (EventCode::RequiredWeeklyTest).as_ref());
/// assert_eq!("Required Weekly Test", (EventCode::RequiredWeeklyTest).as_display_str());
/// assert_eq!(
///     "Required Weekly Test",
///     format!("{}", EventCode::RequiredWeeklyTest)
/// );
/// ```
///
/// All events are mapped to a [significance level](enum.SignificanceLevel.html).
/// This may be useful when deciding how to handle the event.
///
/// ```
/// # use sameold::{EventCode, SignificanceLevel};
///
/// let lvl = (EventCode::RequiredWeeklyTest).to_significance_level();
/// assert_eq!(lvl, SignificanceLevel::Test);
/// ```
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Hash,
    strum_macros::EnumMessage,
    strum_macros::EnumString,
    strum_macros::EnumProperty,
    strum_macros::IntoStaticStr,
    strum_macros::EnumIter,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum EventCode {
    /// Emergency Action Notification (begins national activation)
    #[strum(
        serialize = "EAN",
        detailed_message = "Emergency Action Notification",
        props(level = "W")
    )]
    EmergencyActionNotification,

    /// National Information Center (part of national activation)
    #[strum(
        serialize = "NIC",
        detailed_message = "National Information Center",
        props(level = "S")
    )]
    NationalInformationCenter,

    /// National Periodic Test
    #[strum(serialize = "NPT", detailed_message = "National Periodic Test")]
    NationalPeriodicTest,

    /// Required Monthly Test
    #[strum(serialize = "RMT", detailed_message = "Required Monthly Test")]
    RequiredMonthlyTest,

    /// Required Weekly Test
    #[strum(serialize = "RWT", detailed_message = "Required Weekly Test")]
    RequiredWeeklyTest,

    /// Administrative Message (state/local)
    #[strum(serialize = "ADM", detailed_message = "Administrative Message")]
    AdministrativeMessage,

    /// Avalanche Watch
    #[strum(serialize = "AVA", detailed_message = "Avalanche Watch")]
    AvalancheWatch,

    /// Avalanche Warning
    #[strum(serialize = "AVW", detailed_message = "Avalanche Warning")]
    AvalancheWarning,

    /// Blizzard Warning
    #[strum(serialize = "BZW", detailed_message = "Blizzard Warning")]
    BlizzardWarning,

    /// Blue Alert (state/local)
    #[strum(serialize = "BLU", detailed_message = "Blue Alert", props(level = "W"))]
    BlueAlert,

    /// Child Abduction Emergency (state/local)
    #[strum(serialize = "CAE", detailed_message = "Child Abduction Emergency")]
    ChildAbductionEmergency,

    /// Civil Danger Warning (state/local)
    #[strum(serialize = "CDW", detailed_message = "Civil Danger Warning")]
    CivilDangerWarning,

    /// Civil Emergency Message (state/local)
    #[strum(
        serialize = "CEM",
        detailed_message = "Civil Emergency Message",
        props(level = "W")
    )]
    CivilEmergencyMessage,

    /// Coastal Flood Warning
    #[strum(serialize = "CFW", detailed_message = "Coastal Flood Warning")]
    CoastalFloodWarning,

    /// Coastal Flood Warning
    #[strum(serialize = "CFA", detailed_message = "Coastal Flood Watch")]
    CoastalFloodWatch,

    /// Dust Storm Warning
    #[strum(serialize = "DSW", detailed_message = "Dust Storm Warning")]
    DustStormWarning,

    /// Earthquake Warning
    #[strum(serialize = "EQW", detailed_message = "Earthquake Warning")]
    EarthquakeWarning,

    /// Evacuation Immediate
    #[strum(
        serialize = "EVI",
        detailed_message = "Evacuation Immediate",
        props(level = "W")
    )]
    EvacuationImmediate,

    /// Extreme Wind Warning
    #[strum(serialize = "EWW", detailed_message = "Extreme Wind Warning")]
    ExtremeWindWarning,

    /// Fire Warning
    #[strum(serialize = "FRW", detailed_message = "Fire Warning")]
    FireWarning,

    /// Flash Flood Warning
    #[strum(serialize = "FFW", detailed_message = "Flash Flood Warning")]
    FlashFloodWarning,

    /// Flash Flood Watch
    #[strum(serialize = "FFA", detailed_message = "Flash Flood Watch")]
    FlashFloodWatch,

    /// Flash Flood Statement
    #[strum(serialize = "FFS", detailed_message = "Flash Flood Statement")]
    FlashFloodStatement,

    /// Flood Warning
    #[strum(serialize = "FLW", detailed_message = "Flood Warning")]
    FloodWarning,

    /// Flood Watch
    #[strum(serialize = "FLA", detailed_message = "Flood Watch")]
    FloodWatch,

    /// Flood Statement
    #[strum(serialize = "FLS", detailed_message = "Flood Statement")]
    FloodStatement,

    /// Hazardous Materials Warning
    #[strum(serialize = "HMW", detailed_message = "Hazardous Materials Warning")]
    HazardousMaterialsWarning,

    /// High Wind Warning
    #[strum(serialize = "HWW", detailed_message = "High Wind Warning")]
    HighWindWarning,

    /// High Wind Watch
    #[strum(serialize = "HWA", detailed_message = "High Wind Watch")]
    HighWindWatch,

    /// Hurricane Warning
    #[strum(serialize = "HUW", detailed_message = "Hurricane Warning")]
    HurricaneWarning,

    /// Hurricane Watch
    #[strum(serialize = "HUA", detailed_message = "Hurricane Watch")]
    HurricaneWatch,

    /// Hurricane Statement
    #[strum(serialize = "HLS", detailed_message = "Hurricane Statement")]
    HurricaneStatement,

    /// Law Enforcement Warning
    #[strum(serialize = "LEW", detailed_message = "Law Enforcement Warning")]
    LawEnforcementWarning,

    /// Local Area Emergency
    #[strum(serialize = "LAE", detailed_message = "Local Area Emergency")]
    LocalAreaEmergency,

    /// Network Message Notification
    #[strum(
        serialize = "NMN",
        detailed_message = "Network Message Notification",
        props(level = "M")
    )]
    NetworkMessageNotification,

    /// 911 Telephone Outage Emergency
    #[strum(serialize = "TOE", detailed_message = "911 Telephone Outage Emergency")]
    TelephoneOutageEmergency,

    /// Nuclear Power Plant Warning
    #[strum(serialize = "NUW", detailed_message = "Nuclear Power Plant Warning")]
    NuclearPowerPlantWarning,

    /// Practice/Demo Warning
    #[strum(
        serialize = "DMO",
        detailed_message = "Practice/Demo Warning",
        props(level = "W")
    )]
    PracticeDemoWarning,

    /// Radiological Hazard Warning
    #[strum(serialize = "RHW", detailed_message = "Radiological Hazard Warning")]
    RadiologicalHazardWarning,

    /// Severe Thunderstorm Warning
    #[strum(
        serialize = "SVR",
        detailed_message = "Severe Thunderstorm Warning",
        props(level = "W")
    )]
    SevereThunderstormWarning,

    /// Severe Thunderstorm Watch
    #[strum(serialize = "SVA", detailed_message = "Severe Thunderstorm Watch")]
    SevereThunderstormWatch,

    /// Severe Weather Statement
    #[strum(serialize = "SVS", detailed_message = "Severe Weather Statement")]
    SevereWeatherStatement,

    /// Shelter In Place Warning
    #[strum(serialize = "SPW", detailed_message = "Shelter In Place Warning")]
    ShelterInPlaceWarning,

    /// Special Marine Warning
    #[strum(serialize = "SMW", detailed_message = "Special Marine Warning")]
    SpecialMarineWarning,

    /// Special Weather Statement
    #[strum(serialize = "SPS", detailed_message = "Special Weather Statement")]
    SpecialWeatherStatement,

    /// Storm Surge Watch
    #[strum(serialize = "SSA", detailed_message = "Storm Surge Watch")]
    StormSurgeWatch,

    /// Storm Surge Warning
    #[strum(serialize = "SSW", detailed_message = "Storm Surge Warning")]
    StormSurgeWarning,

    /// Tornado Warning
    #[strum(
        serialize = "TOR",
        detailed_message = "Tornado Warning",
        props(level = "W")
    )]
    TornadoWarning,

    /// Tornado Watch
    #[strum(serialize = "TOA", detailed_message = "Tornado Watch")]
    TornadoWatch,

    /// Tropical Storm Warning
    #[strum(serialize = "TRW", detailed_message = "Tropical Storm Warning")]
    TropicalStormWarning,

    /// Tropical Storm Watch
    #[strum(serialize = "TRA", detailed_message = "Tropical Storm Watch")]
    TropicalStormWatch,

    /// Tsunami Warning
    #[strum(serialize = "TSW", detailed_message = "Tsunami Warning")]
    TsunamiWarning,

    /// Tsunami Watch
    #[strum(serialize = "TSA", detailed_message = "Tsunami Watch")]
    TsunamiWatch,

    /// Volcano Warning
    #[strum(serialize = "VOW", detailed_message = "Volcano Warning")]
    VolcanoWarning,

    /// Winter Storm Warning
    #[strum(serialize = "WSW", detailed_message = "Winter Storm Warning")]
    WinterStormWarning,

    /// Winter Storm Warning
    #[strum(serialize = "WSA", detailed_message = "Winter Storm Watch")]
    WinterStormWatch,
}

impl EventCode {
    /// Obtain event's significance level
    ///
    /// The significance level ranges from "`Test`"
    /// (i.e., "this is only a test") to "`Warning`." Each
    /// event code has a significance level associated with
    /// it. The [`SignificanceLevel`](enum.SignificanceLevel.html)
    /// is useful for determining whether an event merits a
    /// "noisy" and/or "immediate" alert for the message.
    pub fn to_significance_level(&self) -> SignificanceLevel {
        SignificanceLevel::try_from(self).expect("missing significance level definition")
    }

    /// Human-readable string representation
    ///
    /// Converts to a human-readable string, like "`Required Monthly Test`."
    pub fn as_display_str(&self) -> &'static str {
        self.get_detailed_message()
            .expect("missing human-readable definition")
    }

    /// SAME string representation
    ///
    /// Returns the three-character SAME code for this
    /// `EventCode`.
    pub fn as_str(&self) -> &'static str {
        self.get_serializations()[0]
    }
}

impl TryFrom<&str> for EventCode {
    type Error = UnrecognizedEventCode;

    /// Convert from three-character SAME event code
    ///
    /// Converts an event code like "`SVR`" into its enumerated
    /// type (`EventCode::SevereThunderstormWarning`).
    ///
    /// If the code is unrecognized, an error is returned.
    /// An error here does **NOT** mean that the message is
    /// invalid or should be discarded. Instead, if the
    /// error is
    /// [`WithSignificance`](enum.UnrecognizedEventCode.html#variant.WithSignificance),
    /// then you should treat it as a valid (but unknown)
    /// message at the given significance level. This will help
    /// your application react correctly if new codes are
    /// added in the future.
    fn try_from(inp: &str) -> Result<Self, Self::Error> {
        Self::from_str(inp).map_err(|_| UnrecognizedEventCode::from(inp))
    }
}

impl From<&EventCode> for SignificanceLevel {
    /// Convert to significance level
    fn from(evt: &EventCode) -> SignificanceLevel {
        // if we define a level property, use that
        // if the last character is valid, use that
        // otherwise, it's a warning.
        let lvl = evt
            .get_str("level")
            .unwrap_or_else(|| evt.get_serializations()[0]);

        SignificanceLevel::try_from(lvl).expect("missing significance level definition")
    }
}

impl AsRef<str> for EventCode {
    fn as_ref(&self) -> &'static str {
        self.as_str()
    }
}

impl fmt::Display for EventCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_display_str().fmt(f)
    }
}

/// An unrecognized SAME event code
///
/// Even if the complete event code is unknown, the parser may
/// still be able to extract some meaning from it. Most new
/// messages end in the
/// [`SignificanceLevel`](enum.SignificanceLevel.html). A new
/// "Derecho Warning" message, with fictitious code "`DEW`,"
/// still ends in `W` for Warning. Your client application
/// should react to it accordingly as a life-threatening Warning,
/// even if your parser doesn't know what it is.
///
/// If the code is unknown and can't be coerced to any of the
/// `SignificanceLevel`, then
/// `UnrecognizedEventCode::Unrecognized` is returned.
#[derive(Error, Debug, Clone, PartialEq, Eq)]

pub enum UnrecognizedEventCode {
    /// A completely unrecognizable event code
    #[error("Unrecognized")]
    Unrecognized,

    #[error("Unrecognized {0}")]
    WithSignificance(SignificanceLevel),
}

impl From<&str> for UnrecognizedEventCode {
    /// Convert from a SAME event code
    ///
    /// Accepts either a three-character SAME event code or
    /// a single-character significance level. Decodes it
    /// as a `SignificanceLevel` if possible.
    fn from(inp: &str) -> Self {
        match SignificanceLevel::try_from(inp) {
            Ok(sl) => Self::WithSignificance(sl),
            Err(_) => Self::Unrecognized,
        }
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    use strum::IntoEnumIterator;

    #[test]
    fn test_event_api() {
        // Conversion to the Unrecognized… series
        let evt = EventCode::try_from("!!!");
        assert_eq!(Err(UnrecognizedEventCode::Unrecognized), evt);
        assert_eq!("Unrecognized", &format!("{}", evt.err().unwrap()));

        let evt2 = EventCode::try_from("??W").unwrap_err();
        assert_eq!(
            UnrecognizedEventCode::WithSignificance(SignificanceLevel::Warning),
            evt2
        );
        assert_eq!("Unrecognized Warning", &format!("{}", evt2));

        assert_eq!(
            UnrecognizedEventCode::WithSignificance(SignificanceLevel::Statement),
            EventCode::try_from("SSS").unwrap_err()
        );

        // Conversion from string and significance level
        let evt = EventCode::try_from("CEM").unwrap();
        assert_eq!(EventCode::CivilEmergencyMessage, evt);
        assert_eq!(SignificanceLevel::Warning, evt.to_significance_level());
        assert_eq!("Civil Emergency Message", evt.as_display_str());

        let evt = EventCode::try_from("NPT").unwrap();
        assert_eq!(EventCode::NationalPeriodicTest, evt);
        assert_eq!(SignificanceLevel::Test, evt.to_significance_level());
        assert_eq!("National Periodic Test", evt.as_display_str());

        let evt = EventCode::try_from("TOR").unwrap();
        assert_eq!(EventCode::TornadoWarning, evt);
        assert_eq!(SignificanceLevel::Warning, evt.to_significance_level());
        assert_eq!("Tornado Warning", evt.as_display_str());
    }

    #[test]
    fn test_event_completeness() {
        // Did we define our list of Events correctly? This method helps us
        // check them all
        const REQUIRE_NUM_LIVE_CODES: u8 = 56;
        assert_eq!(
            REQUIRE_NUM_LIVE_CODES,
            EventCode::WinterStormWatch as u8 + 1
        );

        let mut code_set =
            std::collections::HashSet::with_capacity(REQUIRE_NUM_LIVE_CODES as usize);
        let mut name_set =
            std::collections::HashSet::with_capacity(REQUIRE_NUM_LIVE_CODES as usize);

        for evt in EventCode::iter() {
            // make sure code assignment and name assignments are unique
            let eee: &str = evt.clone().into();
            assert!(code_set.insert(eee.to_owned()));
            assert!(name_set.insert(evt.as_ref().to_owned()));

            // convert from code
            let cmp = EventCode::from_str(eee).expect("can't back-convert event EEE code!");
            assert_eq!(cmp, evt);

            // convert to significance level does not panic
            let _ = evt.to_significance_level();
        }
    }
}
