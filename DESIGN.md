## Design information for developers

This document summarizes the digital signal processing which takes place in
the `sameold` crate. The top-level receiver is in `receiver.rs`.

### High-rate processing

A DC blocker (`dcblock.rs`) removes DC bias that may result from analog audio
connections. An automatic gain control (`agc.rs`) algorithm then normalizes the
input power to a fixed amplitude range. These pre-processors improve the
performance of the symbol synchronizer, which makes certain assumptions about
the signals it is receiving.

### Demodulation and Downsampling

The demodulator (`demod.rs`) is responsible for turning waveforms into symbols.
In binary constellations like 2FSK, the symbols are -1 and +1. The demodulator
uses a matched filter for each of the two frequencies and compares their output
powers non-coherently.

The demodulator also downsamples to a low-speed sampling rate. The low-speed
rate is approximately twice the SAME baud rate (2× 520.83 Hz). Everything else
in the system happens at this low rate. The demodulator is cleverly designed to
avoid calculating output samples that will just be discarded later.

### Low-rate processing

For optimum performance, the demodulator must sample the signal at exactly the
center of the transmitted symbol. On its own, the demodulator does not know when
this occurs. The symbol synchronizer (`symsync.rs`) is responsible for
estimating the correct sampling time. The demodulator is tightly integrated with
the symbol synchronizer.

The synchronizer uses a feedback algorithm, known as a zero crossing detector,
to estimate when the transition between symbols occurs. The output of the symbol
synchronizer is used to adjust the receiver's downsampling clock. The low-speed
signal is also output.

The individual bits which make up a SAME message must be assembled together into
characters—in this case, ASCII bytes. The code squelch (`codesquelch.rs`) takes
synchronized symbols and attempts to turn them into synchronized bytes. It
listens for a preamble sequence which precedes every SAME transmission. The
preamble sequence is crafted specifically to aid in acquiring the bit and byte
timing.

The code squelch also suppresses (i.e., squelches) its output when no SAME
signal is present. It contains a power detector to prevent garbage symbols from
"noise" (i.e., the normal program audio a station transmits) from tripping the
detector. The code squelch also helps detect the end of each transmission.

Byte-synchronized symbols are then provided to the adaptive equalizer. Up until
this point, the "bits" of a SAME transmission are represented as floating-point
samples. Some parts of the system may make their own internal decisions about
whether the symbol is -1 or +1, but these decisions are not propagated. The
adaptive equalizer is responsible for making the final decision for each symbol.
The adaptive equalizer is a decision-feedback equalizer that contains an
adaptive filter. The adaptive filter can compensate for inter-sample
interference (ISI) and non-flat channel frequency response. The output of the
adaptive filter is a byte stream.

The bytes are then provided to a framer (`framing.rs`). Like all framers, this
framer is responsible for detecting when messages begin or end.

### Transport-layer processing

SAME messages are repeated three times for redundancy. The assemble
(`assembler.rs`) is responsible for combining up to three bursts together into
a single `Message` estimate. The assembler performs 2-of-3 bitwise parity
correction, where applicable, and produces fully-formed `Message` estimates
for the client.
