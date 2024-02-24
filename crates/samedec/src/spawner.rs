//! Spawns child process from a MessageHeader

#![allow(dead_code)]

use std::ffi::OsStr;
use std::io;
use std::process::{Child, Command, Stdio};

use chrono::{DateTime, Utc};
use sameold::MessageHeader;

/// Spawn a child process to handle the given message
///
/// The child process will receive information about the
/// SAME message via the environment. Higher-level logic
/// should pipe streaming audio for the message to the
/// child's stdin.
///
/// This method will attempt to start an executable named
/// `cmd` with the given `args`. The `msg` and `input_rate_str`
/// (the input sampling rate) are transformed into many
/// different environment variables.
pub fn spawn<C, A, B>(
    cmd: C,
    args: A,
    header: &MessageHeader,
    input_rate_str: &str,
) -> io::Result<Child>
where
    C: AsRef<OsStr>,
    B: AsRef<OsStr>,
    A: IntoIterator<Item = B>,
{
    let (issue_ts, purge_ts) = match header.issue_datetime(&Utc::now()) {
        Ok(issue_ts) => (
            time_to_unix_str(issue_ts),
            time_to_unix_str(issue_ts + header.valid_duration()),
        ),
        Err(_e) => ("".to_owned(), "".to_owned()),
    };

    let locations: Vec<&str> = header.location_str_iter().collect();

    Command::new(cmd)
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .args(args)
        .env(childenv::SAMEDEC_RATE, input_rate_str)
        .env(childenv::SAMEDEC_MSG, header.as_str())
        .env(childenv::SAMEDEC_ORG, header.originator_str())
        .env(
            childenv::SAMEDEC_ORIGINATOR,
            header.originator().as_display_str(),
        )
        .env(childenv::SAMEDEC_EVT, header.event_str())
        .env(childenv::SAMEDEC_EVENT, header.event().to_string())
        .env(
            childenv::SAMEDEC_SIGNIFICANCE,
            header.event().significance().as_code_str(),
        )
        .env(childenv::SAMEDEC_LOCATIONS, locations.join(" "))
        .env(childenv::SAMEDEC_ISSUETIME, issue_ts)
        .env(childenv::SAMEDEC_PURGETIME, purge_ts)
        .spawn()
}

mod childenv {
    /// Decoder input rate
    ///
    /// The audio input `--rate` that samedec is running at. This is also
    /// the rate at which samples are output to child processes.
    pub const SAMEDEC_RATE: &str = "SAMEDEC_RATE";

    /// The complete SAME header
    ///
    /// ```txt
    /// ZCZC-EAS-RWT-012057-012081-012101-012103-012115+0030-2780415-WTSP/TV-
    /// ```
    pub const SAMEDEC_MSG: &str = "SAMEDEC_MSG";

    /// SAME originator (code)
    ///
    /// The SAME originator code, like `EAS`.
    pub const SAMEDEC_ORG: &str = "SAMEDEC_ORG";

    /// SAME originator (human-readable)
    ///
    /// A human-readable string for the originator code, like
    /// "`EAS Participant`" for originator code `EAS`.
    pub const SAMEDEC_ORIGINATOR: &str = "SAMEDEC_ORIGINATOR";

    /// SAME event code (three-character string)
    ///
    /// The three-character SAME event code, like `RWT`""
    pub const SAMEDEC_EVT: &str = "SAMEDEC_EVT";

    /// SAME event, human readable
    ///
    /// A human-readable representation of the SAME code, if known.
    /// Example: "`Required Weekly Test`."
    ///
    /// If the event code is unknown, this string will be
    /// "`Unrecognized`".
    pub const SAMEDEC_EVENT: &str = "SAMEDEC_EVENT";

    /// SAME event significance level
    ///
    /// The significance level assigned to the SAME event code,
    /// if known. If the event code is unknown, this string will
    /// be empty.
    ///
    /// Significance levels are assigned by the `sameold`
    /// developers.
    ///
    /// * `T`: Test
    /// * `S`: Statement
    /// * `E`: Emergency
    /// * `A`: Watch
    /// * `W`: Warning
    pub const SAMEDEC_SIGNIFICANCE: &str = "SAMEDEC_SIGNIFICANCE";

    /// FIPS code locations
    ///
    /// Area(s) affected by the message, as a space-delimited list
    /// of six-digit numbers. Example
    ///
    /// ```txt
    /// 012057 012081 012101 012103 012115
    /// ```
    pub const SAMEDEC_LOCATIONS: &str = "SAMEDEC_LOCATIONS";

    /// Message issue time (UTC UNIX timestamp, in seconds)
    ///
    /// The issue time is calculated from the current OS realtime
    /// clock. It will be empty if a complete timestamp cannot be
    /// calculated.
    pub const SAMEDEC_ISSUETIME: &str = "SAMEDEC_ISSUETIME";

    /// Message purge time (UTC UNIX timestamp, in seconds)
    ///
    /// The purge time is calculated from the current OS realtime
    /// clock. It will be empty if a complete timestamp cannot be
    /// calculated.
    pub const SAMEDEC_PURGETIME: &str = "SAMEDEC_PURGETIME";
}

// convert DateTime to UTC unix timestamp in seconds, as string
fn time_to_unix_str(tm: DateTime<Utc>) -> String {
    format!("{}", tm.format("%s"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_time_to_unix_str() {
        let dt: DateTime<Utc> = DateTime::parse_from_rfc2822("Wed, 18 Feb 2015 23:16:09 GMT")
            .unwrap()
            .into();
        assert_eq!(time_to_unix_str(dt), "1424301369");
    }
}
