//! # sameplace: A SAME/EAS Message Parser
//!
//! This crate provides a text parser for
//! [Specific Area Message Encoding](https://en.wikipedia.org/wiki/Specific_Area_Message_Encoding)
//! (SAME). It provides machine- and human-friendly representations
//! of these messages, with [event codes](crate::eventcodes) and
//! [significance levels](crate::SignificanceLevel).
//!
//! For a complete CLI binary, see
//! [`samedec`](https://crates.io/crates/samedec).
//!
//! ## Anatomy of a SAME message
//!
//! SAME/EAS messages contain:
//!
//! 1. A digital header which provides machine-readable information
//! 2. An audio voice message, for human consumption
//! 3. A digital trailer which denotes the end of message.
//!
//! The actual "message" part of a SAME message is the audio itself,
//! which describes the event and provides instructions to the
//! listener. The digital headers do **not** contain all the
//! information as the voice message.
//!
//! * For analog→digital decoding, see our companion library
//!   [`sameold`](https://docs.rs/sameold/latest/sameold/).
//!
//! * For a complete program, which can also handle the voice
//!   message, see our companion binary crate
//!   [`samedec`](https://crates.io/crates/samedec).
//!
//! ## Interpreting Messages
//!
//! The [`MessageHeader`] type decodes a SAME header, like:
//!
//! ```txt
//! ZCZC-WXR-RWT-012345-567890-888990+0015-0321115-KLOX/NWS-
//! ```
//!
//! ```
//! # use std::fmt;
//! use sameplace::{MessageHeader, Originator, Phenomenon, SignificanceLevel};
//!
//! // decode the header string
//! let hdr = MessageHeader::new(
//!     "ZCZC-WXR-RWT-012345-567890-888990+0015-0321115-KLOX/NWS-"
//! ).expect("fail to parse");
//!
//! // what organization originated the message?
//! assert_eq!(Originator::NationalWeatherService, hdr.originator());
//!
//! // parse SAME event code `RWT`
//! let evt = hdr.event();
//!
//! //   the Phenomenon describes what is occurring
//! assert_eq!(Phenomenon::RequiredWeeklyTest, evt.phenomenon());
//!
//! //   the SignificanceLevel indicates the overall severity and/or
//! //   how intrusive or noisy the alert should be
//! assert_eq!(SignificanceLevel::Test, evt.significance());
//! assert!(SignificanceLevel::Test < SignificanceLevel::Warning);
//!
//! //   Display to the user
//! assert_eq!("Required Weekly Test", &format!("{}", evt));
//!
//! // location codes are accessed by iterator
//! let first_location = hdr.location_str_iter().next();
//! assert_eq!(Some("012345"), first_location);
//! ```
//!
//! ## Crate features
//!
//! * `chrono`: Use chrono to calculate message
//!   [issuance times](crate::MessageHeader#method.issue_datetime)
//!   and other fields as true UTC timestamps. If enabled, `chrono`
//!   becomes part of this crate's public API.
//!
//! ## MSRV Policy
//!
//! A minimum supported rust version (MSRV) increase will be treated as a minor
//! version bump.
//!
//! ## Contributing
//!
//! Please read our
//! [contributing guidelines](https://github.com/cbs228/sameold/blob/master/CONTRIBUTING.md)
//! before opening any issues or PRs.

#![deny(unsafe_code)]
#![warn(missing_docs)]

pub mod eventcodes;
mod message;

pub use message::{
    EventCode, InvalidDateErr, Message, MessageDecodeErr, MessageHeader, MessageResult, Originator,
    Phenomenon, SignificanceLevel,
};
