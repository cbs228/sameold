//! Combines multiple bursts into a message estimate
//!
//! See [`combine()`]

use std::convert::TryFrom;

#[cfg(not(test))]
use log::debug;

#[cfg(test)]
use std::println as debug;

use arrayvec::ArrayVec;

use super::assembler::Burst;
use crate::{Message, MessageResult};

/// Convert bursts into a fully-parsed SAME message
///
/// Accepts an iterable container of byte slices, with
/// one container entry per available burst. If:
///
/// 1. a message bytes estimated
/// 2. the error-correction rules permit us to
///    build a message; *and*
/// 3. the bytes parse as a [`Message`]
///
/// then the parsed message is returned. If the estimated
/// bytes fail to parse as a message, an error is returned.
///
/// A `None` value indicates that nothing of consequence happened.
pub fn combine<'b, B, S>(bursts: B) -> Option<MessageResult>
where
    B: IntoIterator<Item = &'b S>,
    S: AsRef<[u8]> + 'b + ?Sized,
{
    const MIN_BURSTS_FOR_FULL_MESSAGE: u8 = 2;

    let (msg, burst_count, bit_errors) = estimate_message(bursts);
    if msg.is_empty() {
        return None;
    }

    debug!(
        "combiner: combined burst data: \"{}\"",
        String::from_utf8_lossy(&msg)
    );

    // limit to bytes which come from 2+ bursts
    let good_msg =
        truncate_bytes_with_reference(&msg[..], &burst_count[..], MIN_BURSTS_FOR_FULL_MESSAGE);
    match Message::try_from((good_msg, bit_errors.as_slice(), burst_count.as_slice())) {
        Ok(parsed) => {
            debug!(
                "combiner: message ({} voting, {} errors): \"{}\"",
                parsed.voting_byte_count(),
                parsed.parity_error_count(),
                parsed
            );
            Some(Ok(parsed))
        }
        Err(err) => {
            // try to parse a Fast EOM
            if message_prefix_is_eom(&msg[..]) {
                debug!("combiner: Fast EOM");
                Some(Ok(Message::EndOfMessage))
            } else if good_msg.is_empty() {
                // there wasn't anything to parse anyway
                None
            } else {
                debug!(
                    "combiner: decode failure ({}): \"{}\"",
                    err,
                    String::from_utf8_lossy(&good_msg)
                );
                Some(Err(err))
            }
        }
    }
}

// Is the given byte an allowed SAME/EAS character?
//
// Allowed characters include the following ASCII:
// - Uppercase letters
// - Lowercase letters
// - Numbers
// - Minus sign (`-`)
// - Plus sign (`+`)
// - Question mark (`?`)
// - Open parentheses (`(`)
// - Close parentheses (`)`)
// - Open brackets (`[`)
// - Close brackets (`]`)
// - Period (`.`)
// - Underscore (`_`)
// - Comma (`,`)
// - Slash (`/`)
// - Space (` `)(might be encountered in callsign field)
//
// The preamble byte `0xAB` is not allowed after the
// data transmission begins, and it is not marked
// as allowed here.
#[inline]
pub(crate) fn is_allowed_byte(c: u8) -> bool {
    const MINUS: u8 = '-' as u8;
    const PLUS: u8 = '+' as u8;
    const QUESTION_MARK: u8 = '?' as u8;
    const OPEN_PARENTHESES: u8 = '(' as u8;
    const CLOSE_PARENTHESES: u8 = ')' as u8;
    const OPEN_BRACKETS: u8 = '[' as u8;
    const CLOSE_BRACKETS: u8 = ']' as u8;
    const PERIOD: u8 = '.' as u8;
    const UNDERSCORE: u8 = '_' as u8;
    const COMMA: u8 = ',' as u8;
    const SLASH: u8 = '/' as u8;
    const SPACE: u8 = ' ' as u8;
    const NUMBERS: [u8; 2] = ['0' as u8, '9' as u8];
    const UPPER_ALPHA: [u8; 2] = ['A' as u8, 'Z' as u8];
    const LOWER_ALPHA: [u8; 2] = ['a' as u8, 'z' as u8];

    c == MINUS
        || (c >= NUMBERS[0] && c <= NUMBERS[1])
        || (c >= UPPER_ALPHA[0] && c <= UPPER_ALPHA[1])
        || (c >= LOWER_ALPHA[0] && c <= LOWER_ALPHA[1])
        || c == SLASH
        || c == QUESTION_MARK
        || c == OPEN_PARENTHESES
        || c == CLOSE_PARENTHESES
        || c == OPEN_BRACKETS
        || c == CLOSE_BRACKETS
        || c == PERIOD
        || c == UNDERSCORE
        || c == COMMA
        || c == PLUS
        || c == SPACE
}

// Combine three bursts into one message
//
// Accepts an iterable container of byte slices, with
// one container entry per available burst. Estimates the
// complete SAME message and returns:
//
// 0. The message bytes
// 1. The total number of bursts used to make each byte; and
// 2. The total number of bit errors in each byte.
//
// All returned vectors will have the same length.
//
// Estimation may be performed on any number of bursts,
// including one. If there are no bursts at all, or no valid
// SAME characters, returns empty arrays
fn estimate_message<'b, B, S>(bursts: B) -> (Burst, Burst, Burst)
where
    B: IntoIterator<Item = &'b S>,
    S: AsRef<[u8]> + 'b + ?Sized,
{
    let mut out_bytes = Burst::default();
    let mut out_num_bursts = Burst::default();
    let mut out_errs = Burst::default();

    // grab up to three bursts from the source
    let mut byte_iters = ArrayVec::<_, 3>::default();
    byte_iters.extend(bursts.into_iter().take(3).map(|b| b.as_ref().into_iter()));

    // we need at least two bursts to proceed
    let mut cur_byte_in = ArrayVec::<u8, 3>::default();
    while out_bytes.len() < out_bytes.capacity() {
        // populate our current input byte with
        // the next byte from all available bursts
        cur_byte_in.clear();
        cur_byte_in.extend(byte_iters.iter_mut().filter_map(|itr| itr.next().cloned()));

        // SAME bytes never have the MSb set. Mask it, but
        // record it as an error
        let mut have_msb_error = false;
        for b in &mut cur_byte_in {
            have_msb_error |= (*b & 0x80) != 0;
            *b = *b & (!0x80);
        }

        // how many estimates do we have?
        let (est_byte, bit_err_count) = match &cur_byte_in.len() {
            0 => break,
            1 => (cur_byte_in[0], 0),
            2 => bit_vote_detect(cur_byte_in[0], cur_byte_in[1]),
            3 => bit_vote_correct(cur_byte_in[0], cur_byte_in[1], cur_byte_in[2]),
            _ => unreachable!(),
        };

        // is the byte valid?
        if !is_allowed_byte(est_byte) {
            break;
        }

        out_bytes.push(est_byte);
        out_num_bursts.push(cur_byte_in.len() as u8);
        out_errs.push(bit_err_count as u8 + have_msb_error as u8);
    }

    (out_bytes, out_num_bursts, out_errs)
}

// Two-of-two bit voting
//
// Assume b0…b1 are multiple repetitions of the
// same byte. For each byte, check that all bytes
// agree. Returns the byte if it agrees or the zero
// byte if it does not. Also returns the number of
// bit errors.
//
// This is a really dumb error-correction scheme.
// You shouldn't use it. Seriously. Don't!
#[inline]
fn bit_vote_detect(b0: u8, b1: u8) -> (u8, u32) {
    // Zeros indicate agreement
    // Ones indicate disputes
    let xor = b0 ^ b1;

    (b0 & !(0xff * (xor != 0) as u8), xor.count_ones())
}

// Two-of-three bit voting
//
// Assume b0…b2 are multiple repetitions of the
// same byte. For each bit, pick the most popular
// state (set or unset). Return the corrected byte
// and the total number of bits with disagreements.
//
// This is a really dumb error-correction scheme.
// You shouldn't use it. Seriously. Don't!
#[inline]
fn bit_vote_correct(b0: u8, b1: u8, b2: u8) -> (u8, u32) {
    // inverse-XOR together. Ones indicate agreement.
    // zeroes indicate disputes.
    let pair0 = !(b0 ^ b1);
    let pair1 = !(b1 ^ b2);
    let pair2 = !(b0 ^ b2);

    // now pair0 is 1 where we want to take from the
    // first pair, pair1 is 1 where we want to
    // take from the second pair, and pair2 is 1
    // where we want to take from the third pair
    (
        (b0 & pair0) | (b2 & pair1) | (b2 & pair2),
        (pair0 & pair1 & pair2).count_zeros(),
    )
}

#[inline]
fn message_prefix_is_eom(inp: &[u8]) -> bool {
    if inp.len() < 2 {
        return false;
    }

    &inp[0..2] == ['N' as u8, 'N' as u8]
}

// for clipping `src` bytes to a `threshold` number of bursts
#[inline]
fn truncate_bytes_with_reference<'a>(src: &'a [u8], compare: &'_ [u8], threshold: u8) -> &'a [u8] {
    let mut ind = 0;
    for (v, _) in compare.iter().copied().zip(src.iter()) {
        if v < threshold {
            break;
        }
        ind += 1;
    }
    &src[0..ind]
}

#[cfg(test)]
mod tests {
    use crate::MessageDecodeErr;

    use super::*;

    #[test]
    fn test_bit_vote_detect() {
        // these are easy
        assert_eq!(bit_vote_detect(0xab, 0xab), (0xab, 0));
        assert_eq!(bit_vote_detect(0xff, 0xff), (0xff, 0));
        assert_eq!(bit_vote_detect(0x00, 0x00), (0x00, 0));

        // introduce disagreement
        assert_eq!(bit_vote_detect(0x00, 0x01), (0x00, 1));
        assert_eq!(bit_vote_detect(0x02, 0x01), (0x00, 2));
        assert_eq!(bit_vote_detect(0xff, 0xf0), (0x00, 4));
        assert_eq!(bit_vote_detect(0x0f, 0xf0), (0x00, 8));
        assert_eq!(bit_vote_detect(0xff, 0x00), (0x00, 8));
    }

    #[test]
    fn test_bit_vote_correct() {
        // these are easy
        assert_eq!(bit_vote_correct(0xab, 0xab, 0xab), (0xab, 0));
        assert_eq!(bit_vote_correct(0xff, 0xff, 0xff), (0xff, 0));
        assert_eq!(bit_vote_correct(0x00, 0x00, 0x00), (0x00, 0));

        // mixed bits
        assert_eq!(bit_vote_correct(0xaa, 0xab, 0xab), (0xab, 1));
        assert_eq!(bit_vote_correct(0xa0, 0xa0, 0xaf), (0xa0, 4));
        assert_eq!(bit_vote_correct(0x0f, 0xf0, 0xff), (0xff, 8));
        assert_eq!(bit_vote_correct(0x00, 0xf0, 0xff), (0xf0, 8));
        assert_eq!(bit_vote_correct(0xaa, 0x55, 0xff), (0xff, 8));
        assert_eq!(bit_vote_correct(0xaa, 0x55, 0xa5), (0xa5, 8));
    }

    #[test]
    fn test_truncate_bytes_with_reference() {
        assert_eq!("".as_bytes(), truncate_bytes_with_reference(&[], &[], 2));
        assert_eq!(
            "X".as_bytes(),
            truncate_bytes_with_reference("XA".as_bytes(), &[2, 1], 2)
        );
        assert_eq!(
            "XB".as_bytes(),
            truncate_bytes_with_reference("XB".as_bytes(), &[3, 2], 2)
        );
        assert_eq!(
            "".as_bytes(),
            truncate_bytes_with_reference("ZZ".as_bytes(), &[1, 2], 2)
        );
    }

    #[test]
    fn test_estimate_message() {
        // one burst -> nothing
        let (msg, mbur, merr) = estimate_message(["".as_bytes()].iter());
        assert!(msg.is_empty());
        assert!(mbur.is_empty());
        assert!(merr.is_empty());

        // two bursts, but exhausted before we can actually do anything
        let (msg, mbur, merr) = estimate_message(["@@".as_bytes(), "".as_bytes()].iter());
        assert!(msg.is_empty());
        assert!(mbur.is_empty());
        assert!(merr.is_empty());

        // two bursts
        let (msg, mbur, merr) = estimate_message(["HIHI".as_bytes(), "HI".as_bytes()].iter());
        let mstr = std::str::from_utf8(msg.as_slice()).unwrap();
        assert_eq!("HIHI", mstr);
        assert_eq!(&[2u8, 2, 1, 1], mbur.as_slice());
        assert_eq!(&[0u8; 4], merr.as_slice());

        // two bursts, halting when one returns a mismatched
        let (msg, mbur, merr) =
            estimate_message(["TEST".as_bytes(), "TESZ".as_bytes(), "".as_bytes()].iter());
        let mstr = std::str::from_utf8(msg.as_slice()).unwrap();
        assert_eq!("TES", mstr);
        assert_eq!(&[2u8, 2, 2], mbur.as_slice());
        assert_eq!(&[0u8, 0, 0], merr.as_slice());

        // three bursts, with bit voting
        // N is 0b01001110
        // Z is 0b01011010
        // C is 0b01000011
        let (msg, mbur, merr) =
            estimate_message(["NNNN".as_bytes(), "NNNN".as_bytes(), "ZCZC-".as_bytes()].iter());
        let mstr = std::str::from_utf8(msg.as_slice()).unwrap();
        assert_eq!("NNNN-", mstr);
        assert_eq!(&[3u8, 3, 3, 3, 1], mbur.as_slice());
        assert_eq!(&[2u8, 3, 2, 3, 0], merr.as_slice());

        // starting off with bit voting, but falling back to error detection
        let (msg, mbur, merr) =
            estimate_message(["NNNN".as_bytes(), "NNNNB".as_bytes(), "ZC".as_bytes()].iter());
        let mstr = std::str::from_utf8(msg.as_slice()).unwrap();
        assert_eq!("NNNNB", mstr);
        assert_eq!(&[3u8, 3, 2, 2, 1], mbur.as_slice());
        assert_eq!(&[2u8, 3, 0, 0, 0], merr.as_slice());

        // check high bit masking
        let (msg, mbur, merr) = estimate_message([&[0xce, 'N' as u8], "NN".as_bytes()].iter());
        let mstr = std::str::from_utf8(msg.as_slice()).unwrap();
        assert_eq!("NN", mstr);
        assert_eq!(&[2u8, 2], mbur.as_slice());
        assert_eq!(&[1u8, 0], merr.as_slice());

        let (msg, mbur, merr) =
            estimate_message([&[0xce, 'N' as u8], "NN".as_bytes(), &['N' as u8, 0xce]].iter());
        let mstr = std::str::from_utf8(msg.as_slice()).unwrap();
        assert_eq!("NN", mstr);
        assert_eq!(&[3u8, 3], mbur.as_slice());
        assert_eq!(&[1u8, 1u8], merr.as_slice());
    }

    #[test]
    fn test_combine() {
        const MESSAGE: &[u8] = "ZCZC-EAS-DMO-999000+0015-0011122-NOCALL00-".as_bytes();
        const CORRUPT: &[u8] = "ZKZK-EAS-DMO-999000+0015-0011122-NOCALL00-".as_bytes();
        const GARBAGE: &[u8] = "NOPE".as_bytes();
        const FAST_EOM_ONLY: &[u8] = "NNZZ".as_bytes();

        // refuse to report a message with only one burst
        assert!(combine([MESSAGE].iter()).is_none());

        // unless it's a Fast EOM
        assert_eq!(
            Ok(Message::EndOfMessage),
            combine([FAST_EOM_ONLY].iter()).expect("expected fast eom")
        );

        // truncate message to parts with 2+ bursts
        // (the truncation causes an error here, which is expected)
        assert_eq!(
            Some(Err(MessageDecodeErr::Malformed)),
            combine([MESSAGE, &MESSAGE[0..16]].iter())
        );

        // garbage message → error
        assert_eq!(
            Err(MessageDecodeErr::UnrecognizedPrefix),
            combine([GARBAGE, GARBAGE].iter()).expect("expected error")
        );

        // good message → parsed
        let out = combine([MESSAGE, MESSAGE].iter())
            .expect("expect output")
            .expect("expected message");
        assert_eq!(out.as_str().as_bytes(), MESSAGE);
        assert_eq!(out.voting_byte_count(), 0);

        // good message with bad burst → parsed
        let out = combine([MESSAGE, MESSAGE, CORRUPT].iter())
            .expect("expect output")
            .expect("expected message");
        assert_eq!(out.as_str().as_bytes(), MESSAGE);
        assert_eq!(out.voting_byte_count(), MESSAGE.len());
        assert_eq!(out.parity_error_count(), 2);

        // an EOM is outvoted in favor of a message, so no
        // Fast EOM is issued
        let out = combine([FAST_EOM_ONLY, MESSAGE, MESSAGE].iter())
            .expect("expect output")
            .expect("expected message");
        assert_eq!(out.as_str().as_bytes(), MESSAGE);
        assert_eq!(out.voting_byte_count(), FAST_EOM_ONLY.len());
    }
}
