# CHANGELOG

## samedec-0.4.1: Maintenance Release

This minor maintenance release helps keep our dependencies
up-to-date and building smoothly with the latest Rust.

BREAKING

* Due to low demand, CI binaries are no longer built for `-musl`
  Linux ABIs. We continue to support `-gnu` ABIs, which offer
  the best performance when available. We continue to require
  glibc >= 2.28, which makes our binaries compatible with
  Debian 10 (buster), AlmaLinux 8, and more modern distros. If
  you require a musl build, open an issue.

* Update MSRVs:

  * `sameold`: 1.70
  * `samedec`: 1.74

ENHANCEMENTS

* cargo: update dependencies to the latest-available versions.
  Minor refactors were necessary.

* ci: new builds for:

    * `aarch64-apple-darwin` (for M1 macs)
    * `i686-unknown-linux-gnu`

* doc: add [CHANGELOG.md](./CHANGELOG.md)

FIXES

* sameold: permit dead code for Demod::push()

* ci: Linux non-x86 architectures are now built by cross-compiling
  instead of emulated "native" toolchains. This is *much* faster.
  We have created custom container images for this purpose.

## samedec-0.4.0: The Spring 2024 Update

Are you "Weather Ready?" This release is mostly compatible with
previous versions but adds more SAME codes and environment
variables.

ENHANCEMENTS

* Support the following new SAME codes:

  * EAN: Renamed to National Emergency Message
  * NAT: National Audible Test
  * NST: National Silent Test
  * FSW: Flash Freeze Warning
  * FZW: Freeze Warning
  * HLS: Hurricane Local Statement
  * SQW: Snow Squall Warning

* Discontinue the `SAMEDEC_SIGNIFICANCE` level of "`M`"
  (Message), which is not found anywhere in the standards
  document. The following messages are upgraded to
  Statement:

  * ADR: Administrative Message
  * NMN: Network Message Notification

* Improved detection of National Weather Service vs
  Environment Canada. samedec will no longer output a
  generic `SAMEDEC_ORIGINATOR` of "`Weather Service`."
  Instead, samedec will output either
  "`National Weather Service`" or "`Environment Canada`."

* New environment variables for child processes

  * `SAMEDEC_IS_NATIONAL`: "`Y`" for national activations;
    otherwise present but empty

  * `SAMEDEC_SIG_NUM`: a numeric representation of the
    significance level

* Add proper integration test scripts for child processes

FIXES

* IMPORTANT: Fix code entry for ADR (Administrative Message),
  which was previously wrong.

* Handle EOF within the main app. Child processes are now
  spawned even for very short input files.

* Update to Edition 2021

* Replace is-terminal with terminal_size

## samedec-0.3.0: Add fallback decoding

ENHANCEMENTS

* CLI-BREAKING: Suppress duplicate messages which follow
  each other quickly in time. This mainly affects Fast EOMs:
  now only one "`NNNN`" line will be printed per SAME
  message. Some users may consider this breaking.

* CLI: Add fallback decoding which can decode SAME headers using
  only two bursts. The improved decoder adds a delay of
  **1.311 seconds** when reporting a Start Of Message. Since the
  SAME voice message is often prefixed with an extended period of
  silence and/or a Warning Alarm Tone, the additional delay
  should not impact most applications.

FIXES

* Fix decoding of back-to-back SAME messages when either or both
  have missed bursts

* Update dependencies

## samedec-0.2.5: CLI Improvements

ENHANCEMENTS

* Update to chrono 0.4.23 and replace
  deprecated functions.

* Update to clap v4. Improve help text and
  general CLI behaviors.

FIXES

* samedec now errors on startup if reading
  from stdin and stdin is a terminal

## samedec-0.2.4: Maintenance release

CI binaries are now available on the Github
Releases tab. These binaries are built with
Github Actions on Github Workers.

ENHANCEMENTS

* Log the full text of each SAME burst

FIXES

* RUSTSEC-2022-0078: bump bumpalo to 3.12.0

* RUSTSEC-2021-0047: replace slice-deque with
  slice-ring-buffer

* Documentation improvements

## samedec-0.2.3: Message bugfix release

FIXES

* Support additional message characters
* Support lowercase event codes
* Use an 8-character callsign for
  built-in DMO messages.

## samedec-0.2.2: Dependency update

No source code changes are made.

* Discontinue use of the `time` dependency of `chrono`, which has
  unresolved security issues [1].

* Update all dependencies.

References
1. https://rustsec.org/advisories/RUSTSEC-2020-0071

## samedec-0.2.1: Dependency update

FIXES

* Update dependencies to versions which correct
  security vulnerabilities. Previous versions
  of sameold are *not* known to be vulnerable.

## samedec-0.2.0: Fast end-of-message detection

ENHANCEMENTS

* BREAKING: Output one `NNNN` line for every
  EOM/`NNNN` which successfully decodes. The
  previous behavior was to require all three
  bursts before outputting a single `NNNN`.
  The behavior for the header (`ZCZC-`) is
  unchanged. The `--demo` option mimics the
  new behavior.

FIXES

* `--help` documentation

## samedec-0.1.0: Initial version

* Decodes SAME signals from `i16` inputs
* Dispatches alerts and streaming audio to child
  processes
