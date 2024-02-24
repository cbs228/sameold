//! SAME/EAS Event Codes

use std::fmt;

use strum::{EnumMessage, EnumProperty};

/// SAME message phenomenon
///
/// A Phenomenon code indicates what prompted the message. These include
/// tests, such as the
/// [required weekly test](Phenomenon::RequiredWeeklyTest),
/// and live messages like [floods](Phenomenon::Flood). Some events
/// have multiple significance levels: floods can be reported as both
/// a "Flood Watch" and a "Flood Warning." The `Phenomenon` only encodes
/// `Phenomenon::Flood`—the [significance](crate::SignificanceLevel)
/// is left to other types.
///
/// Phenomenon may be matched individually if the user wishes to take
/// special action…
///
/// ```
/// # use sameold::Phenomenon;
/// # let phenomenon = Phenomenon::Flood;
/// match phenomenon {
///     Phenomenon::Flood => println!("this message describes a flood"),
///     _ => { /* pass */ }
/// }
/// ```
///
/// … but the programmer must **exercise caution** here. Flooding may also
///  result from a [`Phenomenon::FlashFlood`] or a larger event like a
/// [`Phenomenon::Hurricane`]. An evacuation might be declared with
/// [`Phenomenon::Evacuation`], but many other messages might prompt an
/// evacuation as part of the response. So:
///
/// **⚠️ When in doubt, play the message and let the user decide! ⚠️**
///
/// sameold *does* separate Phenomenon into broad categories. These include:
///
/// ```
/// # use sameold::Phenomenon;
/// assert!(Phenomenon::NationalPeriodicTest.is_national());
/// assert!(Phenomenon::NationalPeriodicTest.is_test());
/// assert!(Phenomenon::SevereThunderstorm.is_weather());
/// assert!(Phenomenon::Fire.is_non_weather());
/// ```
///
/// All Phenomenon `Display` a human-readable description of the event,
/// without its significance level.
///
/// ```
/// # use sameold::Phenomenon;
/// use std::fmt;
///
/// assert_eq!(format!("{}", Phenomenon::HazardousMaterials), "Hazardous Materials");
/// assert_eq!(Phenomenon::HazardousMaterials.as_brief_str(), "Hazardous Materials");
/// ```
///
/// but you probably want to display the full
/// [`EventCode`](crate::EventCode) instead.
///
/// NOTE: the strum traits on this type are **not** considered API.
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Hash,
    strum_macros::EnumMessage,
    strum_macros::EnumProperty,
    strum_macros::EnumIter,
)]
#[non_exhaustive]
pub enum Phenomenon {
    /// National Emergency Message
    ///
    /// This was previously known as Emergency Action Notification
    #[strum(
        message = "National Emergency",
        detailed_message = "National Emergency Message",
        props(national = "")
    )]
    NationalEmergency,

    /// National Information Center (United States, part of national activation)
    #[strum(message = "National Information Center", props(national = ""))]
    NationalInformationCenter,

    /// National Audible Test (Canada)
    #[strum(message = "National Audible Test", props(national = "", test = ""))]
    NationalAudibleTest,

    /// National Periodic Test (United States)
    #[strum(message = "National Periodic Test", props(national = "", test = ""))]
    NationalPeriodicTest,

    /// National Silent Test (Canada)
    #[strum(message = "National Silent Test", props(national = "", test = ""))]
    NationalSilentTest,

    /// Required Monthly Test
    #[strum(message = "Required Monthly Test", props(test = ""))]
    RequiredMonthlyTest,

    /// Required Weekly Test
    #[strum(message = "Required Weekly Test", props(test = ""))]
    RequiredWeeklyTest,

    /// Administrative Message
    ///
    /// Used as follow-up for non-weather messages, including potentially
    /// to issue an all-clear.
    #[strum(message = "Administrative Message")]
    AdministrativeMessage,

    /// Avalanche
    #[strum(message = "Avalanche", detailed_message = "Avalanche %")]
    Avalanche,

    /// Blizzard
    #[strum(
        message = "Blizzard",
        detailed_message = "Blizzard %",
        props(weather = "")
    )]
    Blizzard,

    /// Blue Alert (state/local)
    #[strum(message = "Blue Alert")]
    BlueAlert,

    /// Child Abduction Emergency (state/local)
    #[strum(
        message = "Child Abduction",
        detailed_message = "Child Abduction Emergency"
    )]
    ChildAbduction,

    /// Civil Danger Warning (state/local)
    #[strum(message = "Civil Danger", detailed_message = "Civil Danger Warning")]
    CivilDanger,

    /// Civil Emergency Message (state/local)
    #[strum(
        message = "Civil Emergency",
        detailed_message = "Civil Emergency Message"
    )]
    CivilEmergency,

    /// Coastal Flood
    #[strum(
        message = "Coastal Flood",
        detailed_message = "Coastal Flood %",
        props(weather = "")
    )]
    CoastalFlood,

    /// Dust Storm
    #[strum(
        message = "Dust Storm",
        detailed_message = "Dust Storm %",
        props(weather = "")
    )]
    DustStorm,

    /// Earthquake Warning
    ///
    /// **NOTE:** It is unclear if SAME is fast enough to provide timely
    /// notifications of earthquakes.
    #[strum(message = "Earthquake", detailed_message = "Earthquake Warning")]
    Earthquake,

    /// Evacuation Immediate
    #[strum(message = "Evacuation", detailed_message = "Evacuation Immediate")]
    Evacuation,

    /// Extreme Wind
    #[strum(
        message = "Extreme Wind",
        detailed_message = "Extreme Wind %",
        props(weather = "")
    )]
    ExtremeWind,

    /// Fire Warning
    #[strum(message = "Fire", detailed_message = "Fire %")]
    Fire,

    /// Flash Flood
    #[strum(
        message = "Flash Flood",
        detailed_message = "Flash Flood %",
        props(weather = "")
    )]
    FlashFlood,

    /// Flash Freeze (Canada)
    #[strum(
        message = "Flash Freeze",
        detailed_message = "Flash Freeze %",
        props(weather = "")
    )]
    FlashFreeze,

    /// Flood
    #[strum(message = "Flood", detailed_message = "Flood %", props(weather = ""))]
    Flood,

    /// Freeze (Canada)
    #[strum(message = "Freeze", detailed_message = "Freeze %", props(weather = ""))]
    Freeze,

    /// Hazardous Materials (Warning)
    #[strum(
        message = "Hazardous Materials",
        detailed_message = "Hazardous Materials Warning"
    )]
    HazardousMaterials,

    /// High Wind
    #[strum(
        message = "High Wind",
        detailed_message = "High Wind %",
        props(weather = "")
    )]
    HighWind,

    /// Hurricane
    #[strum(
        message = "Hurricane",
        detailed_message = "Hurricane %",
        props(weather = "")
    )]
    Hurricane,

    /// Hurricane Local Statement
    #[strum(message = "Hurricane Local Statement", props(weather = ""))]
    HurricaneLocalStatement,

    /// Law Enforcement Warning
    #[strum(message = "Law Enforcement Warning")]
    LawEnforcementWarning,

    /// Local Area Emergency
    #[strum(message = "Local Area Emergency")]
    LocalAreaEmergency,

    /// Network Message Notification
    #[strum(message = "Network Message Notification")]
    NetworkMessageNotification,

    /// 911 Telephone Outage Emergency
    #[strum(
        message = "911 Telephone Outage",
        detailed_message = "911 Telephone Outage Emergency"
    )]
    TelephoneOutage,

    /// Nuclear Power Plant (Warning)
    #[strum(
        message = "Nuclear Power Plant",
        detailed_message = "Nuclear Power Plant Warning"
    )]
    NuclearPowerPlant,

    /// Practice/Demo Warning
    #[strum(message = "Practice/Demo Warning")]
    PracticeDemoWarning,

    /// Radiological Hazard
    #[strum(
        message = "Radiological Hazard",
        detailed_message = "Radiological Hazard Warning"
    )]
    RadiologicalHazard,

    /// Severe Thunderstorm
    #[strum(
        message = "Severe Thunderstorm",
        detailed_message = "Severe Thunderstorm %",
        props(weather = "")
    )]
    SevereThunderstorm,

    /// Severe Weather Statement
    #[strum(
        message = "Severe Weather",
        detailed_message = "Severe Weather %",
        props(weather = "")
    )]
    SevereWeather,

    /// Shelter In Place
    #[strum(
        message = "Shelter In Place",
        detailed_message = "Shelter In Place Warning"
    )]
    ShelterInPlace,

    /// Snow Squall
    #[strum(
        message = "Snow Squall",
        detailed_message = "Snow Squall %",
        props(weather = "")
    )]
    SnowSquall,

    /// Special Marine
    #[strum(
        message = "Special Marine",
        detailed_message = "Special Marine %",
        props(weather = "")
    )]
    SpecialMarine,

    /// Special Weather Statement
    #[strum(message = "Special Weather Statement", props(weather = ""))]
    SpecialWeatherStatement,

    /// Storm Surge
    #[strum(
        message = "Storm Surge",
        detailed_message = "Storm Surge %",
        props(weather = "")
    )]
    StormSurge,

    /// Tornado Warning
    #[strum(
        message = "Tornado",
        detailed_message = "Tornado %",
        props(weather = "")
    )]
    Tornado,

    /// Tropical Storm
    #[strum(
        message = "Tropical Storm",
        detailed_message = "Tropical Storm %",
        props(weather = "")
    )]
    TropicalStorm,

    /// Tsunami
    #[strum(
        message = "Tsunami",
        detailed_message = "Tsunami %",
        props(weather = "")
    )]
    Tsunami,

    /// Volcano
    #[strum(message = "Volcano", detailed_message = "Volcano Warning")]
    Volcano,

    /// Winter Storm
    #[strum(
        message = "Winter Storm",
        detailed_message = "Winter Storm %",
        props(weather = "")
    )]
    WinterStorm,

    /// Unrecognized phenomenon
    ///
    /// A catch-all for unrecognized event codes which either did not
    /// decode properly or are not known to sameold. If you encounter
    /// an Unrecognized event code in a production message, please
    /// [report it as a bug](https://github.com/cbs228/sameold/issues)
    /// right away.
    #[strum(message = "Unrecognized", detailed_message = "Unrecognized %")]
    Unrecognized,
}

impl Phenomenon {
    /// Describes the event without its accompanying severity information.
    /// For example,
    ///
    /// ```
    /// # use sameold::Phenomenon;
    /// assert_eq!(Phenomenon::RadiologicalHazard.as_brief_str(), "Radiological Hazard");
    /// ```
    ///
    /// as opposed to the full human-readable description of the event code,
    /// "Radiological Hazard *Warning*." If you want the full description,
    /// use [`EventCode`](crate::EventCode) instead.
    pub fn as_brief_str(&self) -> &'static str {
        self.get_message().expect("missing phenomenon message")
    }

    /// True if the phenomenon is associated with a national activation
    ///
    /// Returns true if the underlying event code is *typically* used
    /// for national activations. This includes both live
    /// National Emergency Messages and the National Periodic Test.
    ///
    /// Clients should consult the message's location codes to
    /// determine if the message actually has national scope.
    pub fn is_national(&self) -> bool {
        self.get_str("national").is_some()
    }

    /// True if the phenomenon is associated with tests
    ///
    /// Returns true if the underlying event code is used only for
    /// tests. Test messages do not represent actual, real-world conditions.
    /// Test messages should also have a
    /// [`SignificanceLevel::Test`](crate::SignificanceLevel::Test).
    pub fn is_test(&self) -> bool {
        self.get_str("test").is_some()
    }

    /// True if the represented phenomenon is weather
    ///
    /// In the United States, weather phenomenon codes like
    /// "Severe Thunderstorm Warning" (`SVR`) are typically
    /// only issued by the National Weather Service. The list of
    /// weather event codes is taken from:
    ///
    /// * "National Weather Service Instruction 10-1708," 11 Dec 2017,
    /// <https://www.nws.noaa.gov/directives/sym/pd01017008curr.pdf>
    ///
    /// Not all **natural phenomenon** are considered **weather.**
    /// Volcanoes, avalanches, and wildfires are examples of non-weather
    /// phenomenon that are naturally occurring. The National Weather
    /// Service does not itself issue these types of alerts; they are
    /// generally left to state and local authorities.
    pub fn is_weather(&self) -> bool {
        self.get_str("weather").is_some()
    }

    /// True if the represented phenomenon is not weather
    ///
    /// The opposite of [`Phenomenon::is_weather()`]. The list of
    /// non-weather event codes available for national, state, and/or
    /// local use is taken from:
    ///
    /// * "National Weather Service Instruction 10-1708," 11 Dec 2017,
    /// <https://www.nws.noaa.gov/directives/sym/pd01017008curr.pdf>
    pub fn is_non_weather(&self) -> bool {
        !self.is_weather()
    }

    /// True if the phenomenon is not recognized
    ///
    /// ```
    /// # use sameold::Phenomenon;
    /// assert!(Phenomenon::Unrecognized.is_unrecognized());
    /// ```
    pub fn is_unrecognized(&self) -> bool {
        self == &Self::Unrecognized
    }

    /// True if the phenomenon is recognized
    ///
    /// ```
    /// # use sameold::Phenomenon;
    /// assert!(Phenomenon::TropicalStorm.is_recognized());
    /// ```
    ///
    /// The opposite of [`is_unrecognized()`](Phenomenon::is_unrecognized).
    pub fn is_recognized(&self) -> bool {
        !self.is_unrecognized()
    }

    /// Pattern string for full representation
    ///
    /// Returns a string like "`Tornado %`" that is the full string
    /// representation of a SAME event code, with significance information.
    /// `%` signs should be replaced with a textual representation of
    /// the event code's significance level.
    pub(crate) fn as_full_pattern_str(&self) -> &'static str {
        self.get_detailed_message()
            .unwrap_or_else(|| self.get_message().expect("missing phenomenon message"))
    }
}

impl Default for Phenomenon {
    fn default() -> Self {
        Self::Unrecognized
    }
}

impl std::fmt::Display for Phenomenon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_brief_str().fmt(f)
    }
}

impl AsRef<str> for Phenomenon {
    fn as_ref(&self) -> &str {
        self.as_brief_str()
    }
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use super::*;

    #[test]
    fn test_national() {
        assert!(Phenomenon::NationalEmergency.is_national());
        assert!(Phenomenon::NationalEmergency.is_non_weather());
        assert!(Phenomenon::NationalPeriodicTest.is_national());
        assert!(Phenomenon::NationalPeriodicTest.is_non_weather());
        assert!(!Phenomenon::Hurricane.is_national());
        assert!(Phenomenon::Hurricane.is_weather());
    }

    // all phenomenon have messages and are either tests,
    // weather, or non-weather
    #[test]
    fn test_property_completeness() {
        for phenom in Phenomenon::iter() {
            // these must not panic
            phenom.as_brief_str();
            phenom.as_full_pattern_str();

            if phenom.is_test() || phenom.is_national() {
                assert!(phenom.is_non_weather());
            }
            if phenom.is_weather() {
                assert!(!phenom.is_test());
            }
        }
    }
}
