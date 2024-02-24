# samedec: Yet Another Decoder for SAME/EAS

*Over-the-air weather alerts for your desktop or RPi.*

This binary crate provides a digital demodulator and decoder program for
[Specific Area Message Encoding](https://en.wikipedia.org/wiki/Specific_Area_Message_Encoding)
(SAME). It can be used as a drop-in replacement for
[multimon-ng](https://github.com/EliasOenal/multimon-ng)'s EAS mode in most
cases.

The demodulation and decoding functions are published separately as the
[`sameold`](https://crates.io/crates/sameold) library crate.

## Background

SAME is commonly used to distribute weather alerts in the United States and
Canada. It was originally developed for use with broadcast stations that carry
analog audio signals, such as:

* [NOAA Weather Radio](https://www.weather.gov/nwr/)
* Commercial FM radio broadcast stations
* Commercial television broadcast and cable networks

These stations participate in an emergency alerting network known as the
[Emergency Alert System](https://en.wikipedia.org/wiki/Emergency_Alert_System),
which disseminates alerts to the general public.

SAME messages are transmitted in place of the station's normal programming as an
audio-only message. SAME messages include a digital header which separates them
from the station's normal programming. The digital header is also sent
in-band—encoded with an analog modulation to preserve it. SAME headers are
modulated using two-level frequency-shift keying (FSK) and sent at a baud rate
of 520.83 Hz.

## Disclaimer

This crate is dual-licensed MIT and Apache 2.0. Read these licenses carefully as
they may affect your rights.

This crate has not been certified as a weather radio receiver or for any other
purpose. The author **strongly discourages** its use in any safety-critical
applications. Always have at least two methods available for receiving weather
alerts.

## Getting Started

```bash
cargo install samedec
```

You will first need to recover *baseband audio* from a radio or television
station which broadcasts SAME signals. Not all stations transmit SAME signals,
and not all stations transmit them *all the time*.

* In the United States, [NOAA Weather Radio](https://www.weather.gov/nwr/) (NWR)
  stations reliably transmit SAME signals for weather emergencies like tornadoes
  and hurricanes. These stations operate in the public safety VHF band on
  dial frequencies ranging from 162.400 MHz to 162.550 MHz.

* In Canada,
  [Weatherradio Canada](https://www.canada.ca/en/environment-climate-change/services/weatheradio.html)
  provides a similar service on the same frequency band.

* Some US broadcasters originate and/or re-transmit SAME messages for the
  Emergency Alert System. To find these stations, it is necessary to locate and
  read your state's "Emergency Alert System (EAS) Plan" document. The EAS plan
  will list Local Primary and other participating stations. Broadcast stations
  are not obligated to relay every message and may decline to relay low-severity
  messages.

NWR transmitters are not guaranteed to relay messages from civil authorities,
such as warnings about wildfires, volcanic activity, or law enforcement
emergencies. Your state's EAS Plan document may specify if NWR transmitters will
carry such messages.

To feed `samedec`, obtain the audio signal that you would normally listen to.
You can do this in any number of ways:

### Via Analog Audio

You can use an analog audio "line out" from a hardware receiver, such as a
weather radio, FM radio, or a scanner. Connect the "line out" port to your
soundcard's "line in" jack.

You will need a way to pipe audio from your soundcard into `samedec`. You can
install [`sox`](http://sox.sourceforge.net/) on most platforms:

```bash
rec -q -t raw -r 22.05k -e signed -b 16 -c 1 - | samedec --rate 22050
```

`samedec` takes input as 1-channel (mono), signed 16-bit integers, in
platform-native endianness. This is equivalent to Rust's `i16` format and is
sometimes referred to in your audio drivers as `s16ne`.

The sampling `--rate` you set in `samedec` must match the sampling rate of the
signal you are piping in. `samedec`'s demodulator will be designed for whatever
`--rate` you request, and it can work with a variety of sampling rates. We
recommend using at least `8000` Hz. Higher sampling rates will cause `samedec`
to use more CPU and I/O throughput, but the difference may not be particularly
important on most systems.

On linux, you can obtain piped audio with either
[`parec`](https://manpages.debian.org/testing/pulseaudio-utils/parec.1.en.html)
(PulseAudio) or
[`arecord`](https://manpages.debian.org/testing/alsa-utils/arecord.1.en.html)
(ALSA). Both are preinstalled on most desktop distributions.

```bash
parec --channels 1 --format s16ne --rate 22050 --latency-msec 500 \
    | samedec -r 22050
```

### Via a Software-Defined Radio

Use any compatible SDR software to demodulate and recover passband audio from a
station of interest. You will need a way to pipe passband audio into `samedec`.

* Some programs, like `rtl_fm`, support piping output directly.

* Some programs, like `gqrx`, can output audio via UDP. You can obtain UDP input
  with either
  [`netcat`](https://manpages.debian.org/testing/netcat-traditional/nc.1.en.html)
  or [`socat`](https://manpages.debian.org/testing/socat/socat.1.en.html).

  ```bash
  nc -l -u 7355 | samedec -r 48000
  ```

* For some programs, you may need to create a virtual audio device and direct
  the audio output to that. PulseAudio can do this "out of the box" with
  [`module-null-sink`](https://freedesktop.org/wiki/Software/PulseAudio/Documentation/User/Modules/#module-null-sink).

`samedec` is not an FM demodulator and cannot accept IQ samples. You need to
demodulate the passband audio signal and feed that into samedec.

For FM stations specifically, you will want to use mono-only decoding
(if available) and the correct deemphasis filter.

### General Advice

Regardless of input method, you will need a clean audio input with minimal
noise. Ideally, you should have a signal that is nearly "full quieting," with no
noise or static. SAME lacks modern error correction techniques and was designed
to operate on links with plenty of signal-to-noise ratio. If the station you
are receiving doesn't sound "good," see if you can find a closer one.

Be sure not to overdrive your soundcard or other input device. Check the
incoming signal levels in your sound control panel or other program to ensure
that they are not anywhere close to saturating. SAME digital headers are sent
at no less than 80% modulation, and they may be louder than regular programming.

If the signal is too quiet, it is usually better to increase the volume of the
sending device (i.e., radio) than it is to command large gain or volume levels
with your soundcard. High volume levels may activate amplifiers that add
unwanted noise. You may have to experiment a bit to find the correct volume
levels. If available, use a "line input" port and not a microphone or headset
port.

**Always test your decoding setup!** Stations which transmit EAS messages are
required to transmit at least one message per week. If after a full week of
listening you do not receive at least one message, something with your setup is
broken.

## Console Output

> Decoding a [sample message](https://commons.wikimedia.org/wiki/File:Same.wav)
> from Wikimedia Commons. Running:
>
> ```bash
> sox 'Same.wav' -t raw -r 22.05k -e signed -b 16 -c 1 - | \
>     samedec -r 22050
> ```
>
> should produce the following output:
>
> ```txt
> ZCZC-EAS-RWT-012057-012081-012101-012103-012115+0030-2780415-WTSP/TV-
> NNNN
> ```

When `samedec` receives a SAME message, the message is printed to stdout. The
printout uses the SAME ASCII encoding that is transmitted over the air.

Exactly one message is printed per line. Only messages are printed.

* SAME *headers*, which indicate the beginning of a message, are prefixed with
  `ZCZC`. Some validation is performed to ensure that headers have the correct
  format, but they may still contain invalid dates or unknown event codes.

* SAME *trailers*, which indicate the end of message, are output as `NNNN`.

[`dsame`](https://github.com/cuppa-joe/dsame)
is a python decoder which can produce human-readable text from this output.
The [`sameold`](https://crates.io/crates/sameold) crate also understands how to
parse message fields.

## Modem Behavior

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
**1.311 seconds** on all received headers. This delay is not usually problematic
as most SAME messages are prefixed with a Warning Alarm Tone that is not
information-bearing.

The message *trailers* are not subject to the same error-correction process and
delay as the headers. The end-of-message indicator (`NNNN`) will be printed just
soon as it is received and decoded.

The modem contains duplicate-suppression logic. Identical messages which arrive
within a window of approximately **10.86 seconds** of each other will be
suppressed and not emitted.

## Child Processes

> ```bash
> … | samedec -r 22050 -- play -q -t raw -r 22.05k -e signed -b 16 -c 1 -
> ```
>
> `samedec` can spawn child processes to handle message audio.

Arguments to `samedec` which follow the ending `--` will be interpreted as a
child process to spawn for each SAME message received. The first argument
("`play`" above) is interpreted as an executable name. The usual rules for your
platform apply with regards to `$PATH` discovery and the requirement that the
executable bit be set. The remaining arguments will be passed to the executable,
verbatim, without further interpretation by `samedec`.

One child process is spawned per SAME message received. The child is spawned
just as soon as `samedec` finishes decoding the SAME header.

Child processes receive "passthrough" voice message audio via their standard
input. Input samples which are provided to `samedec` are streamed to the child
process, verbatim, at the input `--rate`. At the conclusion of the voice
message, the child process standard input is closed. Further progress is blocked
until the child terminates.

The example above will play any SAME message received on your system speakers,
via sox's `play` command.

### What Good Is This?

You can use child processes to selectively play back or store SAME message
audio. You can even compress the audio and email it, but beware: SAME messages
can be up to two minutes long. For some emergencies which require quick
response, two minutes is too long to wait.

### Child Environment

The child process receives the following additional environment variables:

* `SAMEDEC_RATE`: The sampling rate that the decoder is running at, expressed
  as a whole number in Hz: `22050`. Very useful for passing on to audio output
  or encoding programs.

* `SAMEDEC_MSG`: The complete SAME header:
  "`ZCZC-EAS-RWT-012057-012081+0030-2780415-WTSP/TV-`"

* `SAMEDEC_ORG`: the three-character SAME originator code, like "`EAS`."

* `SAMEDEC_ORIGINATOR`: a human-readable originator string, like
  "`EAS Participant`"

* `SAMEDEC_EVT`: the three-character SAME event code, like "`RWT`"

* `SAMEDEC_EVENT`: human-readable event description, including its significance
  level: "`Required Weekly Test`." If the event code is not known, and it its
  significance level is also unknown, then this string will be
  "`Unrecognized Warning`."

* `SAMEDEC_SIGNIFICANCE`: one-character significance level. This variable will
  be empty if the significance level could not be determined (i.e., because
  the event code is unknown).

  * `T`: Test
  * `S`: Statement
  * `E`: Emergency
  * `A`: Watch
  * `W`: Warning

* `SAMEDEC_LOCATIONS`: *space-delimited* list of FIPS location codes, which are
  six characters long. Example: "`012057 012081`"

* `SAMEDEC_ISSUETIME`: The message issue time, as a UTC UNIX timestamp, **IF**
  one can be calculated. Example: "`1424301369`," which can be interpreted as
  "Wed, 18 Feb 2015 23:16:09 GMT."
  * If the issue time could not be calculated, this variable will be empty!
  * Since the full issuance time is not present in the message, the
    `samedec` program assumes that the message was received approximately "now,"
    where "now" is determined by your system's real-time clock.
  * Replays of historical messages are **NOT** guaranteed to yield the same
    value.

* `SAMEDEC_PURGETIME`: The message purge time, as a UTC UNIX timestamp, **IF**
  one can be calculated. The same rules apply as `SAMEDEC_ISSUETIME` above.
  Remember: the purge time is the expiration time of the *message* and *not* the
  expected duration of the hazard.

* `SAMEDEC_IS_NATIONAL`: Set to "`Y`" if the message contains a recognized
  national-level event and location code. The message may either be a test or
  an actual emergency. Clients are **strongly encouraged** to always play
  national-level messages and to never provide the option to suppress them.
  For non-national messages, this variable is set to the empty string.

### Design Requirements for Child Processes

`samedec` provides child processes with input samples synchronously, via
blocking calls. Child processes spawned by `samedec` **MUST** have the following
behavior:

1. Children must **read** OR **close** their standard inputs. Failure to do this
   will temporarily block `samedec` from making progress until the child exits.

2. Children which read from standard input must **EXIT** promptly when they
   reach end of file. Failure to do this will temporarily block `samedec` from
   making progress until the child exits.

Child processes should avoid starting long-running foreground jobs which might
block for extended periods of time. The following sections provide examples
which use `bash` scripting. You can use any language you want for the child
process.

The audio streamed to child processes **MAY** contain one or more SAME trailers
(`NNNN`) which follow the voice message. To minimize latency, `samedec` does not
attempt to remove these.

### Example: Ignoring the Input

```bash
#!/bin/bash

# close standard input to ignore it
exec 0>/dev/null

echo "I got a ${SAMEDEC_EVENT}!"
```

Here, we close the standard input to avoid blocking `samedec`. Your script file
must have the execute bit set (`chmod +x …`).

### Example: Conditional Playback

```bash
#!/bin/bash

[ "${SAMEDEC_SIGNIFICANCE}" = "W" ] || exit 0

exec pacat --channels 1 --format s16ne \
        --rate "${SAMEDEC_RATE}" --latency-msec 500 "$@"
```

The above script will use pulseaudio (on linux) to play back any message which
has a significance level of Warning (`W`). We use `exec` to replace the running
shell with `pacat`. `--rate "${SAMEDEC_RATE}"` tells `pacat` what the sampling
rate is. The "`$@`" is a bashism which passes the remaining input arguments to
the script to `pacat` as arguments.

If you name this script `./play_on_warn.sh`, then an example invocation of
`samedec` is:

```bash
sox 'Same.wav' -t raw -r 22.05k -e signed -b 16 -c 1 - | \
  samedec -r 22050 -- ./play_on_warn.sh
```

### Example: Compress and Save

```bash
#!/bin/bash

outfile="$(date +%s)_${SAMEDEC_ORG}_${SAMEDEC_EVT}_${SAMEDEC_SIGNIFICANCE}_${SAMEDEC_ISSUETIME}.ogg"

exec sox -q -t raw -r "${SAMEDEC_RATE}" -e signed -b 16 -c 1 - \
  -t ogg -C-1 "$outfile"
```

Example invocation, assuming the script is named `./save.sh`.

```bash
sox 'Same.wav' -t raw -r 22.05k -e signed -b 16 -c 1 - | \
  samedec -r 22050 -- ./save.sh
```

## Demo Mode

Invoke `samedec` with the `--demo` option to act as if a SAME header with event
code "`DMO`" was received. The message will be printed to the console, and the
child process (if any) will be spawned. The child will run for eight seconds
before being terminated with a SAME "end of message." At the conclusion of the
demo, `samedec` will exit. This mode is useful for testing your child process
and other event handlers.

During the demo, audio fed into `samedec` will be fed through to the child
process. A source of audio is still required to run the demo mode, but you can
pipe in from `/dev/zero` if you want.

The `sameold` library considers the "`DMO`" event code to have a severity level
of Warning (`SAMEDEC_SIGNIFICANCE=W`).

## Debugging and Troubleshooting

This crate includes `pretty_env_logger`. You can request more verbose output
with `-v`. Use up to three times `-vvv` to increase the verbosity level. Log
messages are printed to stderr.

If you have a **recording** of a signal that you think should demodulate, but
doesn't, please open an new issue on
[github](https://github.com/cbs228/sameold). Either attach or link to your
recording.

Please read our
[contributing guidelines](https://github.com/cbs228/sameold/blob/master/CONTRIBUTING.md)
before opening any issues or PRs.
