[binary](https://crates.io/crates/samedec) | [library](https://crates.io/crates/sameold) | [api docs](https://docs.rs/sameold/latest/sameold)

# sameold: SAME/EAS Demodulation

*Over-the-air weather alerts for your desktop or RPi.*

This project provides a digital demodulator and decoder for
[Specific Area Message Encoding](https://en.wikipedia.org/wiki/Specific_Area_Message_Encoding)
(SAME). SAME is commonly used to distribute weather alerts in the United States
and Canada. It was originally developed for use with broadcast stations that
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

This project provides a Rust library and executable for decoding SAME messages.

## Disclaimer

This project is dual-licensed [MIT](./LICENSE-MIT) and
[Apache 2.0](./LICENSE-APACHE). Read these licenses carefully as they may
affect your rights.

This project has not been certified as a weather radio receiver or for any other
purpose. The author **strongly discourages** its use in any safety-critical
applications. Always have at least two methods available for receiving weather
alerts.

## Getting Started

### Binaries

Binary builds are available on the
[releases](https://github.com/cbs228/sameold/releases) page for a variety of
platforms. Linux binaries are currently built against glibc 2.28 and should be
portable to most distros.

### Building

You will need a working
[rust toolchain](https://www.rust-lang.org/learn/get-started).

To build and install binaries from this repository, run

```bash
cargo install --path crates/samedec
samedec --help
```

Then see the binary's [README](./crates/samedec/README.md) for further
instructions.

### Containerized Build

Containerized builds are available for any architecture supported by the
official [`rust`](https://hub.docker.com/_/rust) image. This example uses
`podman`, but similar commands exist in other container managers:

```bash
DOCKER_BUILDKIT=1 podman build . --tag samedec:latest
podman run --rm -it --userns=keep-id samedec:latest
```

Arguments to `run` are `samedec` arguments. See the binary's
[README](./crates/samedec/README.md) for additional documentation.

Container binaries are not published at present.

### Add SAME decoding to your own project

Add the following to your `Cargo.toml`:

```toml
[dependencies]
sameold = "0.2"
```

Then read our
[API documentation](https://docs.rs/sameold/latest/sameold).

## Contributing

If you have a **recording** of a signal that you think should demodulate, but
doesn't, please open an new issue. Either attach or link to your
recording.

Please read our
[contributing guidelines](https://github.com/cbs228/sameold/blob/master/CONTRIBUTING.md)
before opening any issues or PRs.
