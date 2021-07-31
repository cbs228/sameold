//! Originator code

use std::fmt;
use std::str::FromStr;

use strum::EnumMessage;

/// SAME message originator code
///
/// See [Message::originator()](crate::Message#method.originator).
/// Originator codes may be converted `from()` their SAME string
/// representations. Using them `.as_ref()` or via `Display` will
/// show a human-readable string.
///
/// The variants `NationalWeatherService` and `EnvironmentCanada`
/// must be constructed using both the originator string and the
/// station callsign.
///
/// ```
/// use sameold::Originator;
///
/// let orig = Originator::from("WXR");
/// assert_eq!(Originator::WeatherService, orig);
/// assert_eq!("WXR", orig.as_ref());
/// assert_eq!("Weather Service", orig.as_display_str());
/// assert_eq!("Weather Service", &format!("{}", orig));
///
/// assert_eq!(Originator::Unknown, Originator::from("HUH"));
///
/// let orig = Originator::from(("WXR", "KLOX/NWS"));
/// assert_eq!("National Weather Service", orig.as_display_str());
/// assert_eq!("WXR", orig.as_str());
///
/// assert_eq!(Originator::EnvironmentCanada,
///            Originator::from(("WXR", "EC/GC/CA")));
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
    #[strum(serialize = "WXR", detailed_message = "Weather Service")]
    WeatherService,

    /// National Weather Service
    #[strum(disabled, serialize = "WXR")]
    NationalWeatherService,

    /// Environment Canada
    #[strum(disabled, serialize = "WXR")]
    EnvironmentCanada,

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
        match self {
            Originator::NationalWeatherService => "National Weather Service",
            Originator::EnvironmentCanada => "Environment Canada",
            _ => self.get_detailed_message().expect("missing definition"),
        }
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

impl From<(&str, &str)> for Originator {
    fn from(orig_and_call: (&str, &str)) -> Originator {
        match Originator::from_str(orig_and_call.0) {
            Ok(Originator::WeatherService) => {
                if orig_and_call.1.ends_with("/NWS") {
                    Originator::NationalWeatherService
                } else if orig_and_call.1.starts_with("EC/") {
                    Originator::EnvironmentCanada
                } else {
                    Originator::WeatherService
                }
            }
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
