//! # sameold: SAME/EAS Demodulation
//!
//! This crate provides a digital demodulator and decoder for
//! [Specific Area Message Encoding](https://en.wikipedia.org/wiki/Specific_Area_Message_Encoding)
//! (SAME). It can detect the presence of SAME messages in an audio signal
//! and report them to the caller.
//!
//! ## Disclaimer
//!
//! This crate is dual-licensed MIT and Apache 2.0. Read these licenses
//! carefully as they may affect your rights.
//!
//! This crate has not been certified as a weather radio receiver or for any
//! other purpose. The author **strongly discourages** its use in any
//! safety-critical applications. Always have at least two methods available
//! for receiving weather alerts.
//!
//! ## Example
//!
//! You will first need to recover *baseband audio* from a radio or
//! television station which broadcasts SAME signals. Obtain the
//! audio signal that you would normally listen to. You can use
//! either
//!
//! * an audio "line out" jack from a radio, scanner, or other
//!   receiver; OR
//! * a software-defined radio
//!
//! In either case, obtaining the audio is beyond the scope of this
//! crate. To sample your soundcard, try
//! [cpal](https://crates.io/crates/cpal). If you have a stereo
//! signal, mix to mono first. If you are demodulating wideband FM,
//! and your demodulator offers you a choice, choose mono-only
//! demodulation.
//!
//! ```
//! use sameold::{FrameOut, Message, SameReceiverBuilder};
//!
//! # let some_audio_source_iterator = || std::iter::once(0.0f32);
//! #
//! // create a SameReceiver with your audio sampling rate
//! let mut rx = SameReceiverBuilder::new(22050)
//!     .with_agc_bandwidth(0.05)        // AGC bandwidth at symbol rate, < 1.0
//!     .with_squelch_power(0.10, 0.05)  // squelch open/close power, 0.0 < power < 1.0
//!     .with_preamble_max_errors(2)     // bit error limit when detecting sync sequence
//!     .build();
//!
//! // let audiosrc be an iterator which outputs audio samples,
//! // such as a BufReader bound to stdin or a file, in f32
//! // format at the sampling rate (here 22050 Hz)
//! let audiosrc = some_audio_source_iterator();
//! for evt in rx.iter(audiosrc) {
//!     match evt {
//!         FrameOut::Ready(Ok(Message::StartOfMessage(hdr))) => {
//!             println!("begin SAME voice message: {}", hdr);
//!         }
//!         FrameOut::Ready(Ok(Message::EndOfMessage)) => {
//!             println!("end SAME voice message");
//!         }
//!         _ => {}
//!     }
//! }
//! ```
//!
//! The digital receiver is created via a
//! [builder](struct.SameReceiverBuilder.html).
//!
//! The [`SameReceiver`](struct.SameReceiver.html) binds by iterator to any
//! source of `f32` PCM mono (1-channel) audio samples. If you're using `i16`
//! samples (as most sound cards do), you'll need to cast them to `f32`.
//! There is no need to scale them; the AGC will take care of that.
//!
//! The iterator consumes as many samples as possible until the next
//! [`FrameOut`](enum.FrameOut.html)
//! event. Events include all major state changes, such as acquisition of
//! signal and success or failure to decode a SAME header.
//!
//! The example above traps only the events which signal the beginning and
//! end of a SAME message. The actual "message" part of a SAME message is
//! the audio itself, which should contain a voice message that
//! * describes the event; and
//! * provides instructions to the listener.
//!
//! This crate decodes the digital headers which summarize the message.
//! An example header, as received "off the wire" in ASCII format, is:
//!
//! ```txt
//! ZCZC-WXR-RWT-012345-567890-888990+0015-0321115-KLOX/NWS-
//! ```
//!
//! If this was the header string received, then you could decode
//! `hdr` from the previous example as follows:
//!
//! ```
//! # use sameold::{MessageHeader};
//! use sameold::{EventCode, Originator, SignificanceLevel};
//! # let hdr = MessageHeader::new(
//! #     "ZCZC-WXR-RWT-012345-567890-888990+0015-0321115-KLOX/NWS-"
//! # ).expect("fail to parse");
//!
//! // what organization originated the message?
//! assert_eq!(Originator::WeatherService, hdr.originator());
//!
//! // event code
//! // in actual implementations, handle this error gracefully!
//! let evt = hdr.event().expect("unknown event code");
//! assert_eq!(EventCode::RequiredWeeklyTest, evt);
//!
//! // events have a "significance level" which describes how
//! // urgent or actual they are
//! assert_eq!(SignificanceLevel::Test, evt.to_significance_level());
//! assert!(SignificanceLevel::Test < SignificanceLevel::Warning);
//!
//! // location codes are accessed by iterator
//! let first_location = hdr.location_str_iter().next();
//! assert_eq!(Some("012345"), first_location);
//! ```
//!
//! ## Background
//!
//! SAME is commonly used to distribute weather alerts in the United States and
//! Canada. It was originally developed for use with broadcast stations that
//! carry analog audio signals, such as:
//!
//! * NOAA Weather Radio
//! * Commercial FM radio broadcast stations
//! * Commercial television broadcast and cable networks
//!
//! These stations participate in an emergency alerting network known as the
//! [Emergency Alert System](https://en.wikipedia.org/wiki/Emergency_Alert_System),
//! which disseminates alerts to the general public.
//!
//! SAME messages are transmitted in place of the station's normal programming
//! as an audio-only message. SAME messages include a digital header which
//! separates them from the station's normal programming. The digital header is
//! also sent in-band—encoded with an analog modulation to preserve it. SAME
//! headers are modulated using two-level frequency-shift keying (FSK) and sent
//! at a baud rate of 520.83 Hz.
//!
//! ## Crate features
//!
//! * `chrono`: Use chrono to calculate message
//!   [issuance times](struct.MessageHeader.html#method.issue_datetime)
//!   and other fields as true UTC timestamps. If enabled, `chrono`
//!   becomes part of this crate's public API.
//!

#![allow(dead_code)]

mod agc;
mod builder;
mod codesquelch;
mod demod;
mod equalize;
mod filter;
mod framing;
mod message;
mod receiver;
mod samecodes;
mod symsync;
mod waveform;

pub use builder::{EqualizerBuilder, SameReceiverBuilder};
pub use framing::FrameOut;
pub use message::{InvalidDateErr, Message, MessageDecodeErr, MessageHeader};
pub use receiver::{SameReceiver, SourceIter};
pub use samecodes::{
    EventCode, Originator, SignificanceLevel, UnknownSignificanceLevel, UnrecognizedEventCode,
};
