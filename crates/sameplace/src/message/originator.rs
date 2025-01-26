//! Originator code

use std::fmt;

use strum::EnumMessage;

/// SAME message originator code
///
/// See [`MessageHeader::originator()`](crate::MessageHeader::originator).
/// Originator codes may be also parsed from the SAME
/// [org code and callsign](Originator::from_org_and_call):
///
/// ```
/// use sameplace::Originator;
///
/// let orig = Originator::from_org_and_call("WXR", "KLOX/NWS");
/// assert_eq!(Originator::NationalWeatherService, orig);
///
/// // other originators
/// assert_eq!(Originator::Unknown, Originator::from_org_and_call("HUH", ""));
/// assert_eq!("CIV", Originator::CivilAuthority.as_code_str());
/// ```
///
/// Originators Display a human-readable string:
///
/// ```
/// # use sameplace::Originator;
/// # let orig = Originator::from_org_and_call("WXR", "KLOX/NWS");
/// assert_eq!("National Weather Service", orig.as_display_str());
/// assert_eq!("National Weather Service", &format!("{}", orig));
/// assert_eq!("WXR", orig.as_ref());
/// assert_eq!("WXR", &format!("{:#}", orig));
/// ```
///
/// The callsign is required to reliably detect the National Weather Service
/// and/or Environment Canada:
///
/// ```
/// # use sameplace::Originator;
/// assert_eq!(Originator::EnvironmentCanada,
///            Originator::from_org_and_call("WXR", "EC/GC/CA"));
/// assert_eq!("WXR", Originator::EnvironmentCanada.as_code_str());
/// ```
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, Hash, strum_macros::EnumMessage, strum_macros::EnumString,
)]
pub enum Originator {
    /// An unknown (and probably invalid) Originator code
    ///
    /// Per NWSI 10-172, receivers should accept any originator code.
    #[strum(serialize = "", detailed_message = "Unknown Originator")]
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

    /// National Weather Service
    #[strum(serialize = "WXR", detailed_message = "National Weather Service")]
    NationalWeatherService,

    /// Environment Canada
    ///
    /// In Canada, SAME is only transmitted on the Weatheradio Canada
    /// radio network to alert weather radios. SAME signals are not
    /// transmitted on broadcast AM/FM or cable systems.
    ///
    /// This enum variant will only be selected if the sending station's
    /// callsign matches the format of Environment Canada stations.
    #[strum(message = "WXR", detailed_message = "Environment Canada")]
    EnvironmentCanada,

    /// EAS participant (usu. broadcast station)
    #[strum(
        serialize = "EAS",
        detailed_message = "Broadcast station or cable system"
    )]
    BroadcastStation,
}

impl Originator {
    /// Construct from originator string and station callsign
    pub fn from_org_and_call<S1, S2>(org: S1, call: S2) -> Self
    where
        S1: AsRef<str>,
        S2: AsRef<str>,
    {
        let decode = str::parse(org.as_ref()).unwrap_or_default();
        if decode == Self::NationalWeatherService && call.as_ref().starts_with("EC/") {
            Self::EnvironmentCanada
        } else {
            decode
        }
    }

    /// Human-readable string representation
    ///
    /// Converts to a human-readable string, like "`Civil authorities`."
    pub fn as_display_str(&self) -> &'static str {
        self.get_detailed_message().expect("missing definition")
    }

    /// SAME string representation
    ///
    /// Returns the SAME code for this `Originator`.
    /// [`Originator::Unknown`] returns the empty string.
    pub fn as_code_str(&self) -> &'static str {
        self.get_message()
            .unwrap_or_else(|| self.get_serializations()[0])
    }
}

impl std::default::Default for Originator {
    fn default() -> Self {
        Self::Unknown
    }
}

impl AsRef<str> for Originator {
    fn as_ref(&self) -> &'static str {
        self.as_code_str()
    }
}

impl fmt::Display for Originator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.as_code_str().fmt(f)
        } else {
            self.as_display_str().fmt(f)
        }
    }
}
