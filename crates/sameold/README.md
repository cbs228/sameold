# sameold: SAME/EAS Demodulation

*Over-the-air weather alerts for your desktop or RPi.*

This crate provides a digital demodulator and decoder for
[Specific Area Message Encoding](https://en.wikipedia.org/wiki/Specific_Area_Message_Encoding)
(SAME). It can detect the presence of SAME messages in an audio signal
and report them to the caller.

## Disclaimer

This crate is dual-licensed MIT and Apache 2.0. Read these licenses
carefully as they may affect your rights.

This crate has not been certified as a weather radio receiver or for any
other purpose. The author **strongly discourages** its use in any
safety-critical applications. Always have at least two methods available
for receiving weather alerts.

## Example

A complete example may be found in our
[`samedec`](https://crates.io/crates/samedec) crate, which provides a
command-line program for decoding SAME via pipes.

### Demodulation and Decoding

You will first need to recover *baseband audio* from a radio or
television station which broadcasts SAME signals. Obtain the
audio signal that you would normally listen to. You can use
either

* an audio "line out" jack from a radio, scanner, or other
  receiver; OR
* a software-defined radio

In either case, obtaining the audio is beyond the scope of this
crate. To sample your soundcard, try
[cpal](https://crates.io/crates/cpal). If you have a stereo
signal, mix to mono first. If you are demodulating wideband FM,
and your demodulator offers you a choice, choose mono-only
demodulation.

```rust
use sameold::{Message, SameReceiverBuilder};

// Create a SameReceiver with your audio sampling rate
// Sound cards typically run at 44100 Hz or 48000 Hz. Use
// an input rate of at least 8000 Hz.
let mut rx = SameReceiverBuilder::new(48000)
    .with_agc_bandwidth(0.05)        // AGC bandwidth at symbol rate, < 1.0
    .with_agc_gain_limits(1.0/(i16::MAX as f32), 1.0/200.0)  // for i16
    .with_squelch_power(0.10, 0.05)  // squelch open/close power, 0.0 < power < 1.0
    .with_preamble_max_errors(2)     // bit error limit when detecting sync sequence
    .build();

// let audiosrc be an iterator which outputs audio samples,
// such as a BufReader bound to stdin or a file, in f32
// format at the sampling rate (here 48000 Hz)
let audiosrc = some_audio_source_iterator();
for msg in rx.iter_messages(audiosrc) {
    match msg {
        Message::StartOfMessage(hdr) => {
            println!("begin SAME voice message: {}", hdr);
        }
        Message::EndOfMessage => {
            println!("end SAME voice message");
        }
    }
}
```

The digital receiver is created via a
[builder](https://docs.rs/sameold/latest/sameold/struct.SameReceiverBuilder.html).

The [`SameReceiver`](https://docs.rs/sameold/latest/sameold/struct.SameReceiver.html)
binds by iterator to any source of `f32` PCM mono (1-channel) audio samples. If
you're using `i16` samples (as most sound cards do), you'll need to cast them to
`f32`. There is no need to scale them as long as you configure the AGC properly,
as above.

The
[`iter_messages()`](https://docs.rs/sameold/latest/sameold/struct.SameReceiver.html#method.iter_messages)
iterator consumes as many samples as possible until the next
[`Message`](https://docs.rs/sameold/latest/sameold/enum.Message.html)
is decoded.

### Modem Behavior

| # of Bursts | Decoding Strategy                  |
|-------------|------------------------------------|
| 1           | Fast EOM / `NNNN` only             |
| 2           | Error detection (equality checks)  |
| 3           | Error correction (bit voting)      |

SAME messages are always transmitted three times, in separate "bursts," for
redundancy. When decoding the start of message *headers* (`ZCZC`), `samedec`
will use all three bursts together to improve decoding—if possible.

If one retransmission is missed, `samedec` will automatically fall back to
decoding with only two bursts. The decoder imposes a delay of approximately
**1.311 seconds** on all received headers. This delay is not usually
problematic as most SAME messages are prefixed with a Warning Alarm Tone that
is not information-bearing.

The message *trailers* are not subject to the same error-correction process
and delay as the headers. The end-of-message indicator (`NNNN`) will be
printed just soon as it is received and decoded.

The modem contains duplicate-suppression logic. Identical messages which
arrive within a window of approximately **10.86 seconds** of each other will
be suppressed and not emitted.

The modem is separated into two parts:

1. the "*link layer*," which converts analog waveforms into framed
   [`Burst`](https://docs.rs/sameold/latest/sameold/enum.LinkState.html#variant.Burst)s;
   and

2. the "*transport layer*," assembles individual `Bursts` into `Messages`.

Events from both layers can be captured using the
[`iter_events()`](https://docs.rs/sameold/latest/sameold/struct.SameReceiver.html#method.iter_events)
method instead of `iter_messages()`. The events iterator can be used to obtain
raw framed
[bursts](https://docs.rs/sameold/latest/sameold/struct.SameEvent.html#method.burst)
without delay or error-correction. Events can also report the detection of SAME
carrier signals before and during message decoding.

### Interpreting Messages

The [`Message`] type marks the start or end of a SAME message. The
actual "message" part of a SAME message is the audio itself, which
should contain a voice message that

* describes the event; and
* provides instructions to the listener.

This crate decodes the digital headers and trailers which summarize
the message. An example header, as received "off the wire" in ASCII
format, is:

```txt
ZCZC-WXR-RWT-012345-567890-888990+0015-0321115-KLOX/NWS-
```

If this was the header string received, then you could decode
`hdr` from the previous example as follows:

```rust
use sameold::{EventCode, Originator, SignificanceLevel};

// what organization originated the message?
assert_eq!(Originator::NationalWeatherService, hdr.originator());

// event code
// in actual implementations, handle this error gracefully!
let evt = hdr.event().expect("unknown event code");
assert_eq!(EventCode::RequiredWeeklyTest, evt);

// events have a "significance level" which describes how
// urgent or actual they are
assert_eq!(SignificanceLevel::Test, evt.to_significance_level());
assert!(SignificanceLevel::Test < SignificanceLevel::Warning);

// location codes are accessed by iterator
let first_location = hdr.location_str_iter().next();
assert_eq!(Some("012345"), first_location);
```

SAME messages are always transmitted three times for redundancy.
When decoding the message header, `sameold` will use all three
transmissions together to improve decoding. Only one
[`Message::StartOfMessage`] is output for all three header transmissions.
The trailers which denote the end of the message are **not** subject to
this error-correction process. One [`Message::EndOfMessage`] is
output for every trailer received. There may be up to three
`EndOfMessage` output for every complete SAME message.

## Background

SAME is commonly used to distribute weather alerts in the United States and
Canada. It was originally developed for use with broadcast stations that
carry analog audio signals, such as:

* [NOAA Weather Radio](https://www.weather.gov/nwr/)
* Commercial FM radio broadcast stations
* Commercial television broadcast and cable networks

These stations participate in an emergency alerting network known as the
[Emergency Alert System](https://en.wikipedia.org/wiki/Emergency_Alert_System),
which disseminates alerts to the general public.

SAME messages are transmitted in place of the station's normal programming
as an audio-only message. SAME messages include a digital header which
separates them from the station's normal programming. The digital header is
also sent in-band—encoded with an analog modulation to preserve it. SAME
headers are modulated using two-level frequency-shift keying (FSK) and sent
at a baud rate of 520.83 Hz.

## Crate features

* `chrono`: Use chrono to calculate message
  [issuance times](https://docs.rs/sameold/latest/sameold/struct.MessageHeader.html#method.issue_datetime)
  and other fields as true UTC timestamps. If enabled, `chrono`
  becomes part of this crate's public API.

## MSRV Policy

A minimum supported rust version (MSRV) increase will be treated as a minor
version bump.

## Contributing

If you have a **recording** of a signal that you think should demodulate, but
doesn't, please open an new issue on
[github](https://github.com/cbs228/sameold). Either attach or link to your
recording.

Please read our
[contributing guidelines](https://github.com/cbs228/sameold/blob/master/CONTRIBUTING.md)
before opening any issues or PRs.

License: MIT OR Apache-2.0
