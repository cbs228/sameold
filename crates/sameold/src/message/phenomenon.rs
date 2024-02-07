//! SAME/EAS Event Codes

use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;

use strum::{EnumMessage, EnumProperty};
use thiserror::Error;

use crate::SignificanceLevel;

/// SAME message event code
///
/// Usually constructed via
/// [MessageHeader::event()](crate::MessageHeader#method.event).
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
/// All events are mapped to a [significance level](SignificanceLevel).
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
    /// it. The [`SignificanceLevel`](SignificanceLevel)
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
    /// [`WithSignificance`](UnrecognizedEventCode#variant.WithSignificance),
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
/// [`SignificanceLevel`](SignificanceLevel). A new
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

    /// An unknown event code which *does* match a significance level
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
