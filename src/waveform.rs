/// Waveform parameters and matched filters for SAME
use nalgebra::DVector;
use num_complex::Complex;

/// Mark frequency (Hz)
pub const FSK_MARK_HZ: f32 = 2083.3;

/// Space frequency (Hz)
pub const FSK_SPACE_HZ: f32 = 1562.5;

/// Baud rate (Hz)
pub const BAUD_HZ: f32 = 520.83;

/// Preamble byte
///
/// The preamble byte is repeated sixteen times before every
/// SAME message. It contains many bit transitions to ensure
/// that bit and byte synchronization is acquired quickly
pub const PREAMBLE: u8 = 0xab;

/// SAME preamble sync sequence
///
/// The preamble byte is repeated sixteen times before every
/// SAME message. Here, we will use four occurrences of it
/// for synchronization.
pub const PREAMBLE_SYNC_WORD: u32 = u32::from_be_bytes([PREAMBLE, PREAMBLE, PREAMBLE, PREAMBLE]);

/// SAME baud rate at the given sampling frequency, in fractional samples
pub fn samples_per_symbol(fs: u32) -> f32 {
    fs as f32 / BAUD_HZ as f32
}

/// Generate mark and space matched filter taps for SAME
///
/// Generates a tuple of (`mark_taps`,`space_taps`), which are
/// the matched filter for the mark tone and the space tone,
/// respectively. The filter is generated for the given input
/// sampling rate `fs`.
pub fn matched_filter(fs: u32) -> (DVector<Complex<f32>>, DVector<Complex<f32>>) {
    let ntaps = f32::floor(samples_per_symbol(fs)) as usize;
    let mark = cisoid_matched_filter(ntaps, FSK_MARK_HZ / fs as f32);
    let space = cisoid_matched_filter(ntaps, FSK_SPACE_HZ / fs as f32);
    (mark, space)
}

// Generate matched filter taps
//
// These FIR filter taps are a matched filter for a complex
// exponential cisoid at a fixed frequency, `freq_fs`. Specify
// frequency as a fraction of the sampling rate.
//
// The output taps are a time-reversed, complex-conjugated
// cisoid.
fn cisoid_matched_filter(points: usize, freq_fs: f32) -> DVector<Complex<f32>> {
    let mut out = DVector::from_element(points, Complex::new(0.0, 0.0));
    for (iter, o) in out.iter_mut().enumerate() {
        *o = Complex::new(
            0.0,
            2.0 * std::f32::consts::PI * freq_fs as f32 * ((points - 1 - iter) as f32),
        );
        *o = 2.0f32 * o.exp().conj() / points as f32;
    }
    out
}

/// Very simple continuous-phase AFSK modulator
///
/// This method is designed for use in tests. The baud rate
/// is always an even integer number of samples. Accepts
/// symbols. Returns modulated signal and number of samples
/// per symbol.
#[cfg(test)]
pub fn modulate_afsk(syms: &[f32], fs: u32) -> (DVector<f32>, usize) {
    const TWOPI: f32 = 2.0f32 * std::f32::consts::PI;

    let mark_rad_per_sa = TWOPI * FSK_MARK_HZ / (fs as f32);
    let space_rad_per_sa = TWOPI * FSK_SPACE_HZ / (fs as f32);
    let symlen = {
        let symlen = f32::floor(samples_per_symbol(fs)) as usize;
        if symlen % 2 == 0 {
            symlen
        } else {
            symlen + 1
        }
    };

    let mut out = DVector::from_element(syms.len() * symlen, 0.0f32);
    let mut phase = 0.0f32;
    for (itr, sa) in out.iter_mut().enumerate() {
        let sym = syms[itr / symlen] >= 0.0;
        if sym {
            phase += mark_rad_per_sa;
        } else {
            phase += space_rad_per_sa;
        }
        if phase > TWOPI {
            // wrapped
            phase = -TWOPI + phase;
        }
        *sa = phase.cos();
    }

    (out, symlen)
}

/// Convert bytes to symbols
///
/// Converts bytes to SAME symbols. +1 is emitted for one bits,
/// and -1 is emitted for zero bits. The symbols are output in
/// SAME order (least significant bit first).
#[cfg(test)]
pub fn bytes_to_symbols(bytes: &[u8]) -> Vec<f32> {
    let mut v = Vec::with_capacity(bytes.len() * 8);
    for byte in bytes {
        let mut word = *byte;
        for _i in 0..8 {
            let bit = word & 0x01;
            if bit == 1 {
                v.push(1.0f32);
            } else {
                v.push(-1.0f32);
            }
            word = word >> 1;
        }
    }

    v
}

/// Convert bytes to samples with two samples per symbol
///
/// Converts bytes to SAME samples. +1 is emitted for one bits,
/// and -1 is emitted for zero bits. The symbols are output in
/// SAME order (least significant bit first). Two samples are
/// emitted for every symbol.
#[cfg(test)]
pub fn bytes_to_samples(bytes: &[u8]) -> Vec<f32> {
    let mut v = Vec::with_capacity(bytes.len() * 8 * 2);
    for byte in bytes {
        let mut word = *byte;
        for _i in 0..8 {
            let bit = word & 0x01;
            v.push(0.0f32);
            if bit == 1 {
                v.push(1.0f32);
            } else {
                v.push(-1.0f32);
            }
            word = word >> 1;
        }
    }

    v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cisoid_matched_filter() {
        const FREQ_FS: f32 = 0.0944807256f32;
        const EXPECT_REAL: &[f32] = &[-0.719973f32, -0.208581, 0.374184, 0.828910, 1.000000];
        const EXPECT_IMAG: &[f32] = &[-0.694002f32, -0.978005, -0.927355, -0.559382, -0.000000];

        let gain = 2.0f32 / EXPECT_REAL.len() as f32;
        let out = cisoid_matched_filter(EXPECT_REAL.len(), FREQ_FS);
        for (i, item) in out.iter().enumerate() {
            let d = (item - gain * Complex::new(EXPECT_REAL[i], EXPECT_IMAG[i])).norm();
            assert!(d < 1e-4);
        }
    }

    #[test]
    fn test_bytes_to_symbols() {
        // symbol mapping for [0xAB, 0x21]
        const EXPECT_SYMS: &[f32] = &[
            1.0f32, 1.0f32, -1.0f32, 1.0f32, -1.0f32, 1.0f32, -1.0f32, 1.0f32, 1.0f32, -1.0f32,
            -1.0f32, -1.0f32, -1.0f32, 1.0f32, -1.0f32, -1.0f32,
        ];

        const BYTES: &[u8] = &[0xAB, 0x21];

        let syms = bytes_to_symbols(BYTES);
        assert_eq!(EXPECT_SYMS, syms.as_slice());
    }
}
