//! # List of SAME Events Codes Known to `sameold`
//!
//! | `XYZ` | Description                            |
//! |-------|----------------------------------------|
//! | `ADR` | Administrative Message                 |
//! | `AVA` | Avalanche Watch                        |
//! | `AVW` | Avalanche Warning                      |
//! | `BLU` | Blue Alert                             |
//! | `BZW` | Blizzard Warning                       |
//! | `CAE` | Child Abduction Emergency              |
//! | `CDW` | Civil Danger Warning                   |
//! | `CEM` | Civil Emergency Message                |
//! | `CFA` | Coastal Flood Watch                    |
//! | `CFW` | Coastal Flood Warning                  |
//! | `DMO` | Practice/Demo Warning                  |
//! | `DSW` | Dust Storm Warning                     |
//! | `EAN` | National Emergency Message             |
//! | `EQW` | Earthquake Warning                     |
//! | `EVI` | Evacuation Immediate                   |
//! | `EWW` | Extreme Wind Warning                   |
//! | `FFA` | Flash Flood Watch                      |
//! | `FFS` | Flash Flood Statement                  |
//! | `FFW` | Flash Flood Warning                    |
//! | `FLA` | Flood Watch                            |
//! | `FLS` | Flood Statement                        |
//! | `FLW` | Flood Warning                          |
//! | `FRW` | Fire Warning                           |
//! | `FSW` | Flash Freeze Warning                   |
//! | `FZW` | Freeze Warning                         |
//! | `HLS` | Hurricane Local Statement              |
//! | `HMW` | Hazardous Materials Warning            |
//! | `HUA` | Hurricane Watch                        |
//! | `HUW` | Hurricane Warning                      |
//! | `HWA` | High Wind Watch                        |
//! | `HWW` | High Wind Warning                      |
//! | `LAE` | Local Area Emergency                   |
//! | `LEW` | Law Enforcement Warning                |
//! | `NAT` | National Audible Test                  |
//! | `NIC` | National Information Center            |
//! | `NMN` | Network Notification Message           |
//! | `NPT` | National Periodic Test                 |
//! | `NST` | National Silent Test                   |
//! | `NUW` | Nuclear Power Plant Warning            |
//! | `RHW` | Radiological Hazard Warning            |
//! | `RMT` | Required Monthly Test                  |
//! | `RWT` | Required Weekly Test                   |
//! | `SMW` | Special Marine Warning                 |
//! | `SPS` | Special Weather Statement              |
//! | `SPW` | Shelter In-Place warning               |
//! | `SQW` | Snow Squall Warning                    |
//! | `SSA` | Storm Surge Watch                      |
//! | `SSW` | Storm Surge Warning                    |
//! | `SVA` | Severe Thunderstorm Watch              |
//! | `SVR` | Severe Thunderstorm Warning            |
//! | `SVS` | Severe Weather Statement               |
//! | `TOA` | Tornado Watch                          |
//! | `TOE` | 911 Telephone Outage Emergency         |
//! | `TOR` | Tornado Warning                        |
//! | `TRA` | Tropical Storm Watch                   |
//! | `TRW` | Tropical Storm Warning                 |
//! | `TSA` | Tsunami Watch                          |
//! | `TSW` | Tsunami Warning                        |
//! | `VOW` | Volcano Warning                        |
//! | `WSA` | Winter Storm Watch                     |
//! | `WSW` | Winter Storm Warning                   |
//!
//! SAME event codes for the United States are given in
//! [NWSI 10-1712](https://www.nws.noaa.gov/directives/sym/pd01017012curr.pdf).
//!
//! ## See Also
//!
//! * [`EventCode`](crate::EventCode)
//! * [`MessageHeader::event()`](crate::MessageHeader::event)

use phf::phf_map;

use crate::{Phenomenon, SignificanceLevel};

/// An entry in [`CODEBOOK`].
pub(crate) type CodeEntry = (Phenomenon, SignificanceLevel);

/// Lookup a three-character SAME event code in the database
///
/// If the input `code` matches a `CodeEntry` that is known to
/// sameold, returns it. If no exact match could be found, the
/// third character is matched as a significance level only. If
/// even that does not match, returns `None`.
pub(crate) fn parse_event<S>(code: S) -> Option<CodeEntry>
where
    S: AsRef<str>,
{
    let code = code.as_ref();
    if code.len() != 3 {
        // invalid
        return None;
    }

    // try the full three-character code first
    lookup_threecharacter(code)
        // if not, lookup the two-character code + significance
        .or_else(|| lookup_twocharacter(code))
        // if not, is the last character a known SignificanceLevel?
        .or_else(|| lookup_onecharacter(code))
    // otherwise → None
}

/// Database of three-character SAME event codes.
///
/// All three-character codes imply a significance level:
/// the `RWT` will always have a significance of `Test`.
static CODEBOOK3: phf::Map<&'static str, CodeEntry> = phf_map! {
    // national activations
    "EAN" => (Phenomenon::NationalEmergency, SignificanceLevel::Warning),
    "NIC" => (Phenomenon::NationalInformationCenter, SignificanceLevel::Statement),

    // tests
    "DMO" => (Phenomenon::PracticeDemoWarning, SignificanceLevel::Warning),
    "NAT" => (Phenomenon::NationalAudibleTest, SignificanceLevel::Test),
    "NPT" => (Phenomenon::NationalPeriodicTest, SignificanceLevel::Test),
    "NST" => (Phenomenon::NationalSilentTest, SignificanceLevel::Test),
    "RMT" => (Phenomenon::RequiredMonthlyTest, SignificanceLevel::Test),
    "RWT" =>  (Phenomenon::RequiredWeeklyTest, SignificanceLevel::Test),

    // civil authority codes
    "ADR" => (Phenomenon::AdministrativeMessage, SignificanceLevel::Statement),
    "BLU" => (Phenomenon::BlueAlert, SignificanceLevel::Warning),
    "CAE" => (Phenomenon::ChildAbduction, SignificanceLevel::Emergency),
    "CDW" => (Phenomenon::CivilDanger, SignificanceLevel::Warning),
    "CEM" => (Phenomenon::CivilEmergency, SignificanceLevel::Warning),
    "EQW" => (Phenomenon::Earthquake, SignificanceLevel::Warning),
    "EVI" => (Phenomenon::Evacuation, SignificanceLevel::Warning),
    "FRW" => (Phenomenon::Fire, SignificanceLevel::Warning),
    "HMW" => (Phenomenon::HazardousMaterials, SignificanceLevel::Warning),
    "LAE" => (Phenomenon::LocalAreaEmergency, SignificanceLevel::Emergency),
    "LEW" => (Phenomenon::LawEnforcementWarning, SignificanceLevel::Warning),
    "NMN" => (Phenomenon::NetworkMessageNotification, SignificanceLevel::Statement),
    "NUW" => (Phenomenon::NuclearPowerPlant, SignificanceLevel::Warning),
    "RHW" => (Phenomenon::RadiologicalHazard, SignificanceLevel::Warning),
    "SPW" => (Phenomenon::ShelterInPlace, SignificanceLevel::Warning),
    "TOE" => (Phenomenon::TelephoneOutage, SignificanceLevel::Emergency),
    "VOW" => (Phenomenon::Volcano, SignificanceLevel::Warning),

    // weather codes, three-character
    "HLS" => (Phenomenon::HurricaneLocalStatement, SignificanceLevel::Statement),
    "SPS" => (Phenomenon::SpecialWeatherStatement, SignificanceLevel::Statement),
    "SVR" => (Phenomenon::SevereThunderstorm, SignificanceLevel::Warning),
    "SVS" => (Phenomenon::SevereWeather, SignificanceLevel::Statement),
    "TOR" => (Phenomenon::Tornado, SignificanceLevel::Warning),

    //   "flash freeze warning" is Canada-only and not a NWS VTEC code
    "FSW" => (Phenomenon::FlashFreeze, SignificanceLevel::Warning),
};

/// Database of two-character (plus significance) SAME codes
///
/// Two-character codes follow a standard convention set by
/// the National Weather Service: the last character is the
/// significance level.
static CODEBOOK2: phf::Map<&'static str, Phenomenon> = phf_map! {
    // civil authority codes, two-character with standard significance
    "AV" => Phenomenon::Avalanche,

    // weather codes, two-character with standard significance
    "BZ" => Phenomenon::Blizzard,
    "CF" => Phenomenon::CoastalFlood,
    "DS" => Phenomenon::DustStorm,
    "EW" => Phenomenon::ExtremeWind,
    "FF" => Phenomenon::FlashFlood,
    "FL" => Phenomenon::Flood,
    "FZ" => Phenomenon::Freeze,
    "HU" => Phenomenon::Hurricane,
    "HW" => Phenomenon::HighWind,
    "SM" => Phenomenon::SpecialMarine,
    "SQ" => Phenomenon::SnowSquall,
    "SS" => Phenomenon::StormSurge,
    "SV" => Phenomenon::SevereThunderstorm,
    "TO" => Phenomenon::Tornado,
    "TR" => Phenomenon::TropicalStorm,
    "TS" => Phenomenon::Tsunami,
    "WS" => Phenomenon::WinterStorm,
};

/// Get codebook entry for full code like "`RWT`"
fn lookup_threecharacter(code: &str) -> Option<CodeEntry> {
    CODEBOOK3.get(code.get(0..3)?).cloned()
}

/// Convert `BZx` → `CodeEntry` with proper significance
fn lookup_twocharacter(code: &str) -> Option<CodeEntry> {
    let phenom = CODEBOOK2.get(code.get(0..2)?).cloned()?;
    Some((phenom, code.get(2..3)?.into()))
}

/// Convert `??x` → Unrecognized event with parsed significance
fn lookup_onecharacter(code: &str) -> Option<CodeEntry> {
    Some((Phenomenon::Unrecognized, code.get(2..3)?.into()))
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashSet;

    use lazy_static::lazy_static;
    use regex::Regex;
    use strum::IntoEnumIterator;

    /// ensure we have populated our codebooks correctly
    #[test]
    fn check_codebooks() {
        lazy_static! {
            static ref ASCII_UPPER: Regex = Regex::new(r"^[[A-Z]]{2,3}$").expect("bad test regexp");
        }

        let mut codebook_phenomenon = HashSet::new();

        for (key, val) in CODEBOOK3.entries() {
            assert!(key.is_ascii());
            assert_eq!(key.len(), 3);
            ASCII_UPPER.is_match(key);
            assert_ne!(Phenomenon::Unrecognized, val.0);
            assert_ne!(SignificanceLevel::Unknown, val.1);
            codebook_phenomenon.insert(val.0);
        }

        for (key, val) in CODEBOOK2.entries() {
            assert!(key.is_ascii());
            assert_eq!(key.len(), 2);
            ASCII_UPPER.is_match(key);
            assert_ne!(&Phenomenon::Unrecognized, val);
            codebook_phenomenon.insert(*val);
        }

        // check that every Phenomenon is covered by at least one codebook entry
        for phen in Phenomenon::iter() {
            if phen.is_unrecognized() {
                continue;
            }

            assert!(
                codebook_phenomenon.contains(&phen),
                "phenomenon {} not covered by any codebook entries",
                phen
            );
        }
    }
}
