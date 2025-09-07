//! # FIR linear filters
//!
//! The [`FilterCoeff`] implements the
//! multiply-accumulate operation of a Finite Impulse Response
//! filter. FIR filters convolve the input with an impulse
//! response `h` to obtain the output. Convolution consists of
//! two operations:
//!
//! 1. Multiply-accumulate: A window which contains the previous
//!    `h.len()` input samples is multiplied element-wise with `h`.
//!    The sum of all the elements is the output.
//!
//! 2. Sliding window: to advance time to the next output sample,
//!    a new input sample is shifted onto the window. The oldest
//!    output sample is aged off.
//!
//! ## Multiply-Accumulate
//!
//! [`FilterCoeff`] implements only (1). To
//! use, let the input window be a slice like
//!
//! ```txt
//! // the sample L is the youngest sample, and O is the oldest
//! // [ O | N | M | L ]
//! ```
//!
//! to obtain the output for sample `Q`, run
//!
//! ```ignore
//! let coeff = FilterCoeff::from_slice(&[1.0f32]);  // this is the identity filter
//! let history = &[1.0f32, 2.0f32, 3.0f32, 4.0f32]; // sample history slice
//! let out = coeff.filter(history); // out is 4.0f32
//! ```
//!
//! Then shift the slice like this,
//!
//! ```txt
//! // the sample K is now the youngest sample
//! // [ N | M | L | K ]
//! ```
//!
//! and repeat to obtain the output for sample `K`.
//!
//! The input window *should* be the same length as the filter
//! coefficients, but this is not strictly enforced.
//!
//! * If it is shorter, the missing samples are treated as zeros.
//! * If it is longer, the excess samples are ignored.
//!
//! ## Sliding window
//!
//! The [`Window`] class implements a sliding
//! window. New samples are pushed onto the window, and old samples
//! are allowed to age off. The `Window` has a fixed size at
//! construction time.
//!
//! To use `Window` for FIR filtering, create it with the same
//! length as `FilterCoeff`.
//!
//! ```ignore
//! let wind : Window<f32> = Window::new(4);
//! wind.push(&[1.0f32, 2.0f32, 3.0f32, 4.0f32]);
//! ```

use std::collections::VecDeque;
use std::convert::AsRef;

use nalgebra::base::Scalar;
use nalgebra::DVector;
use num_traits::{One, Zero};

/// FIR filter coefficients
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
pub struct FilterCoeff<T>(DVector<T>)
where
    T: Copy + Scalar + One + Zero;

#[allow(dead_code)]
impl<T> FilterCoeff<T>
where
    T: Copy + Scalar + One + Zero,
{
    /// Create from slice
    ///
    /// Creates FIR filter coefficients with the specified impulse
    /// response `h`. The coefficients `h` use the same representation
    /// as GNU Octave's `filter()` function.
    pub fn from_slice<S>(h: S) -> Self
    where
        S: AsRef<[T]>,
    {
        let inp = h.as_ref();
        FilterCoeff(DVector::from_iterator(inp.len(), inp.iter().copied()))
    }

    /// Create an identity filter
    ///
    /// The identity filter is a "no-op" impulse response
    pub fn from_identity(len: usize) -> Self {
        let mut out = FilterCoeff(DVector::from_iterator(
            len,
            std::iter::repeat(T::zero()).take(len),
        ));
        out.0[0] = T::one();
        out
    }

    /// Number of filter coefficients
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Perform FIR filtering with the given sample history
    ///
    /// Computes the current output sample of the filter assuming
    /// the given `history`. `history` must be a
    /// `DoubledEndedIterator` which outputs the oldest sample
    /// first and the newest sample last. The newest sample is
    /// used for feedforward lag 0. `history` SHOULD contain at
    /// least `self.len()` samples, but no error is raised if it
    /// is shorter.
    ///
    /// The caller is encouraged to use a deque for `history`,
    /// but this is not enforced.
    pub fn filter<W, In, Out>(&self, history: W) -> Out
    where
        W: IntoIterator<Item = In>,
        W::IntoIter: DoubleEndedIterator,
        In: Copy + Scalar + std::ops::Mul<T, Output = Out>,
        Out: Copy + Scalar + Zero + std::ops::AddAssign,
    {
        multiply_accumulate(history, self.as_ref())
    }

    /// Reset to identity filter
    ///
    /// The filter coefficients are reset to a "no-op" identity
    /// filter.
    pub fn identity(&mut self) {
        for coeff in self.0.iter_mut() {
            *coeff = T::zero();
        }
        self.0[0] = T::one();
    }

    /// Return filter coefficients as slice
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        self.0.as_slice()
    }

    /// Return filter coefficients as mutable slice
    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        self.0.as_mut_slice()
    }

    /// Obtain filter coefficients
    #[inline]
    pub fn inner(&self) -> &DVector<T> {
        &self.0
    }

    /// Obtain filter coefficients (mutable)
    #[inline]
    pub fn inner_mut(&mut self) -> &mut DVector<T> {
        &mut self.0
    }
}

impl<T> AsRef<[T]> for FilterCoeff<T>
where
    T: Copy + Scalar + One + Zero,
{
    #[inline]
    fn as_ref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T> AsMut<[T]> for FilterCoeff<T>
where
    T: Copy + Scalar + One + Zero,
{
    #[inline]
    fn as_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }
}

impl<T> std::ops::Index<usize> for FilterCoeff<T>
where
    T: Copy + Scalar + One + Zero,
{
    type Output = T;

    #[inline]
    fn index(&self, ind: usize) -> &T {
        self.0.index(ind)
    }
}

impl<T> std::ops::IndexMut<usize> for FilterCoeff<T>
where
    T: Copy + Scalar + One + Zero,
{
    #[inline]
    fn index_mut(&mut self, ind: usize) -> &mut T {
        self.0.index_mut(ind)
    }
}

/// Filter window
///
/// Implements a fixed-size lookback window for FIR filters
/// or other purposes.
#[derive(Clone, Debug)]
pub struct Window<T>(VecDeque<T>)
where
    T: Copy + Scalar + Zero;

#[allow(dead_code)]
impl<T> Window<T>
where
    T: Copy + Scalar + Zero,
{
    /// Create empty window, filling it with zeros
    ///
    /// Creates a new `Window` with the given `len`gth
    pub fn new(len: usize) -> Self {
        let mut q = VecDeque::with_capacity(len);
        q.resize(len, T::zero());
        Self(q)
    }

    /// Reset to zero initial conditions
    ///
    /// Clear the window, filling it with zeros
    pub fn reset(&mut self) {
        for s in &mut self.0 {
            *s = T::zero()
        }
    }

    /// Window length
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Append to sample window
    ///
    /// Appends the `input` slice to the right side of the Window.
    /// The last sample of `input` becomes the right-most / most recent
    /// sample of the Window.
    ///
    /// If the length of `input` exceeds the length of the Window,
    /// then the right-most chunk of `input` will be taken.
    pub fn push<S>(&mut self, input: S)
    where
        S: AsRef<[T]>,
    {
        let input = input.as_ref();
        let input = if input.len() > self.0.len() {
            let start = input.len() - self.0.len();
            &input[start..]
        } else {
            input
        };

        // age off the size of input
        std::mem::drop(self.0.drain(0..input.len()));

        // add new
        self.0.extend(input.as_ref());
    }

    /// Append a scalar to the sample window
    ///
    /// Appends the `input` scalar to the right side of the Window.
    /// It becomes the last / most recent sample of Window.
    /// Returns the sample that was formerly the oldest
    /// sample in the Window.
    #[inline]
    pub fn push_scalar(&mut self, input: T) -> T {
        let out = self.0.pop_front().unwrap_or(T::zero());
        self.0.push_back(input);
        out
    }

    /// Iterator over window contents
    ///
    /// The iterator outputs the least recent sample first.
    /// The most recent sample is output last.
    pub fn iter(&self) -> <&Window<T> as IntoIterator>::IntoIter {
        self.into_iter()
    }

    /// Convert window contents to a vector
    ///
    /// Copy the current contents of the window to a freshly-allocated
    /// vector. Vector elements are ordered from least recent sample
    /// to most recent sample.
    pub fn to_vec(&self) -> Vec<T> {
        self.iter().collect()
    }

    /// Obtain the inner SliceRingBuffer
    pub fn inner(&self) -> &VecDeque<T> {
        &self.0
    }

    /// Most recent element pushed into the Window
    #[inline]
    pub fn back(&self) -> T {
        *self.0.back().unwrap()
    }

    /// Least recent element pushed into the Window
    #[inline]
    pub fn front(&self) -> T {
        *self.0.front().unwrap()
    }
}

impl<'a, T> IntoIterator for &'a Window<T>
where
    T: Copy + Scalar + Zero,
{
    type Item = T;

    type IntoIter = std::iter::Copied<std::collections::vec_deque::Iter<'a, T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().copied()
    }
}

// Multiply-accumulate operation
//
// Calculates the sum of the element-wise multiplication,
//
// ```txt
// out = Σ history[i] * coeff[i]
// ```
//
// This is the core operation of FIR filtering. In an FIR filter,
// `history` contains the sample history. The most recent sample
// is stored in `history[N-1]`, and the least recent sample in the
// history is stored in `history[0]`. The filter coefficients are
// stored in `coeff`, with `coeff[0]` being the zeroth filter
// coefficient.
//
// The two inputs need not be the same length. If `history` is shorter
// than `coeff`, then the sample history is assumed to be zero
// outside of its range.
//
// To perform FIR filtering, new samples are shifted onto the end of
// history, and a `multiply_accumulate()` operation is performed for
// each output sample.
//
// The output value is returned. Any compatible arithmetic types may
// be used, including complex numbers.
fn multiply_accumulate<W, In, Coeff, Out>(history: W, coeff: &[Coeff]) -> Out
where
    W: IntoIterator<Item = In>,
    W::IntoIter: DoubleEndedIterator,
    In: Copy + Scalar + std::ops::Mul<Coeff, Output = Out>,
    Coeff: Copy + Scalar,
    Out: Copy + Scalar + Zero + std::ops::AddAssign,
{
    let history = history.into_iter();
    let mut out = Out::zero();
    for (hi, co) in history.rev().zip(coeff.iter()) {
        out += hi * *co;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    use assert_approx_eq::assert_approx_eq;

    use num_complex::Complex;

    #[test]
    fn test_multiply_accumulate() {
        // trivial MAC is zero
        let out = multiply_accumulate(&[0.0f32; 0], &[0.0f32; 0]);
        assert_eq!(0.0f32, out);

        // simple multiplies; we clip to the end
        let out = multiply_accumulate(&[20.0f32, 1.0f32], &[1.0f32]);
        assert_eq!(1.0f32, out);
        let out = multiply_accumulate(&[1.0f32], &[1.0f32, 20.0f32]);
        assert_eq!(1.0f32, out);

        // more complicated multiply
        let out = multiply_accumulate(&[20.0f32, 20.0f32], &[1.0f32, -1.0f32]);
        assert_approx_eq!(0.0f32, out);
    }

    #[test]
    fn test_filter_cplx() {
        const INPUT: &[Complex<f32>] = &[Complex {
            re: 0.5f32,
            im: 0.5f32,
        }];

        let filter = FilterCoeff::from_slice(&[2.0f32, 0.0f32, 0.0f32]);

        let out = filter.filter(INPUT);
        assert_approx_eq!(out.re, 1.0f32);
        assert_approx_eq!(out.im, 1.0f32);
    }

    #[test]
    fn test_filter_identity() {
        const EXPECT: &[f32] = &[1.0f32, 0.0f32, 0.0f32, 0.0f32];

        let mut filter = FilterCoeff::<f32>::from_identity(4);
        assert_eq!(EXPECT, filter.as_ref());
        assert_eq!(10.0f32, filter.filter(&[10.0f32]));

        filter[3] = 5.0f32;
        filter.identity();
        assert_eq!(EXPECT, filter.as_ref());
    }

    #[test]
    fn test_window() {
        let mut wind: Window<f32> = Window::new(4);
        assert_eq!(4, wind.len());
        assert_eq!(vec![0.0f32, 0.0f32, 0.0f32, 0.0f32], wind.to_vec());
        wind.push(&[1.0f32]);
        assert_eq!(vec![0.0f32, 0.0f32, 0.0f32, 1.0f32], wind.to_vec());
        wind.push(&[]);
        assert_eq!(vec![0.0f32, 0.0f32, 0.0f32, 1.0f32], wind.to_vec());

        wind.push(&[2.0f32]);
        assert_eq!(vec![0.0f32, 0.0f32, 1.0f32, 2.0f32], wind.to_vec());

        wind.push(&[-1.0f32, -2.0f32, 1.0f32, 2.0f32, 3.0f32, 4.0f32]);
        assert_eq!(vec![1.0f32, 2.0f32, 3.0f32, 4.0f32], wind.to_vec());
        assert_eq!(4.0f32, wind.back());
        assert_eq!(1.0f32, wind.front());
        assert_eq!(4, wind.len());

        // push individual samples works too
        assert_eq!(1.0f32, wind.push_scalar(10.0f32));
        assert_eq!(4, wind.len());
        assert_eq!(vec![2.0f32, 3.0f32, 4.0f32, 10.0f32], wind.to_vec());

        // exactly enough to fill
        wind.push(&[5.0f32, 4.0f32, 3.0f32, 2.0f32]);
        assert_eq!(vec![5.0f32, 4.0f32, 3.0f32, 2.0f32], wind.to_vec());

        wind.reset();
        assert_eq!(4, wind.len());
        assert_eq!(vec![0.0f32, 0.0f32, 0.0f32, 0.0f32], wind.to_vec());
    }
}
