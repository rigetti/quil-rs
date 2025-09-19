//! Utilities for working with floating-point numbers that want to have true equality in ℝ ∪ {±∞,
//! NaN}.
//!
//! In particular, the functions in this module will treat `±0.0` as indistiguishable, all NaNs as
//! indistiguishable, and all NaNs as equal only to each other.

/// [`f64`] utilities.
pub(crate) mod f64 {
    use std::hash::{Hash as _, Hasher};

    /// Compares two [`f64`]s for equality such that all `NaN`s are considered equal.  This is
    /// reflexive and so can be used to implement [`Eq`].
    ///
    /// This equality function is compatible with using [`hash`] as a hash function.
    ///
    /// Notes:
    /// * This function, like ordinary `f64` equality, equates `+0.0` and `-0.0`.
    /// * This function, *un*like ordinary `f64` equality, equates all `NaN`s.
    #[inline]
    pub(crate) fn eq(left: f64, right: f64) -> bool {
        left == right || left.is_nan() && right.is_nan()
    }

    /// Hashes an [`f64`] such that all `NaN`s are considered equal.
    ///
    /// This hash function is compatible with using [`eq`] as an equality function.
    ///
    /// Notes:
    /// * This function hashes `+0.0` and `-0.0` to the same value.
    /// * This function hashes all `NaNs` to the same value.
    #[inline]
    pub(crate) fn hash<H: Hasher>(value: f64, state: &mut H) {
        let value = if value == 0.0f64 {
            // `+0.0` and `-0.0` have different bits, but compare equal; we thus hash the bit form of
            // `+0.0` for both, so that `hash_f64(+0.0)` == `hash(-0.0)`.
            0.0f64
        } else if value.is_nan() {
            // There are many different NaNs, and this function wants to support equating them all, so
            // we just hash the standard NaN.
            f64::NAN
        } else {
            value
        };

        value.to_bits().hash(state)
    }

    #[cfg(test)]
    mod test {
        use std::{collections::hash_map::DefaultHasher, hash::Hasher};

        fn hash(float: f64) -> u64 {
            let mut hasher = DefaultHasher::new();
            super::hash(float, &mut hasher);
            hasher.finish()
        }

        #[test]
        fn eq_f64_zeros() {
            let pos = 0.0f64;
            let neg = -0.0f64;
            assert_eq!(pos, neg);
            assert_ne!(pos.to_bits(), neg.to_bits());
            assert!(super::eq(pos, neg));
        }

        #[test]
        fn eq_f64_nan() {
            let nan = f64::NAN;
            assert!(nan.is_nan());
            assert_ne!(nan, nan);
            assert_eq!(nan.to_bits(), nan.to_bits());
            assert!(super::eq(nan, nan));
        }

        #[test]
        fn eq_f64_nans() {
            let nan1 = f64::NAN;
            let nan2 = -f64::NAN;
            assert!(nan1.is_nan() && nan2.is_nan());
            assert_ne!(nan1.to_bits(), nan2.to_bits());
            assert!(super::eq(nan1, nan2));
        }

        #[test]
        fn hash_f64_zeros() {
            let pos = 0.0f64;
            let neg = -0.0f64;
            assert_eq!(pos, neg);
            assert_ne!(pos.to_bits(), neg.to_bits());
            assert_eq!(hash(pos), hash(neg));
        }

        #[test]
        fn hash_f64_nans() {
            let nan1 = f64::NAN;
            let nan2 = -f64::NAN;
            assert!(nan1.is_nan() && nan2.is_nan());
            assert_ne!(nan1.to_bits(), nan2.to_bits());
            assert_eq!(hash(nan1), hash(nan2));
        }
    }
}

/// [`Complex64`] utilities.
///
/// Note that these functions consider the real and imaginary components of the numbers separately;
/// `NaN + 3i` is considered equal to `NaN + 3i` but not `3 + NaN*i`, for instance.
pub(crate) mod complex64 {
    use std::hash::Hasher;

    use num_complex::{Complex, Complex64};

    /// Compares two [`Complex64`]s for equality such that all `NaN`s are considered equal.  This is
    /// reflexive and so can be used to implement [`Eq`].
    ///
    /// This equality function is compatible with using [`hash`] as a hash function.
    ///
    /// Notes:
    /// * This function, like ordinary `Complex64` equality, equates `+0.0` and `-0.0` components.
    /// * This function, *un*like ordinary `Complex64` equality, equates all `NaN` components.
    /// * The real and imaginary components are compared independently; `NaN + 3i` compares equal to
    ///   `NaN + 3i` but not `3 + NaN*i`, for example.
    #[inline]
    pub(crate) fn eq(left: Complex64, right: Complex64) -> bool {
        let Complex {
            re: left_re,
            im: left_im,
        } = left;

        let Complex {
            re: right_re,
            im: right_im,
        } = right;

        super::f64::eq(left_re, right_re) && super::f64::eq(left_im, right_im)
    }

    /// Hashes a [`Complex64`] such that all `NaN`s are considered equal.
    ///
    /// This hash function is compatible with using [`eq`] as an equality function.
    ///
    /// Notes:
    /// * This function hashes `+0.0` and `-0.0` components to the same value.
    /// * This function hashes all `NaN` components to the same value.
    /// * The real and imaginary components are hashed independently; `NaN + 3i` will not
    ///   necessarily hash to the same value as `3 + NaN*i`, for example.
    #[inline]
    pub(crate) fn hash<H: Hasher>(value: Complex64, state: &mut H) {
        let Complex { re, im } = value;
        super::f64::hash(re, state);
        super::f64::hash(im, state);
    }

    #[cfg(test)]
    mod test {
        use std::{collections::hash_map::DefaultHasher, hash::Hasher};

        use num_complex::{c64, Complex, Complex64};

        fn hash(float: Complex64) -> u64 {
            let mut hasher = DefaultHasher::new();
            super::hash(float, &mut hasher);
            hasher.finish()
        }

        fn to_bits(value: Complex64) -> (u64, u64) {
            let Complex { re, im } = value;
            (re.to_bits(), im.to_bits())
        }

        struct Zeros {
            pos_pos: Complex64,
            pos_neg: Complex64,
            neg_pos: Complex64,
            neg_neg: Complex64,
        }

        impl Zeros {
            fn new() -> Self {
                let pos_pos = c64(0.0, 0.0);
                let pos_neg = c64(0.0, -0.0);
                let neg_pos = c64(-0.0, 0.0);
                let neg_neg = c64(-0.0, -0.0);

                assert_eq!(pos_pos, pos_neg);
                assert_eq!(pos_pos, neg_pos);
                assert_eq!(pos_pos, neg_neg);

                assert_ne!(to_bits(pos_pos), to_bits(pos_neg));
                assert_ne!(to_bits(pos_pos), to_bits(neg_pos));
                assert_ne!(to_bits(pos_pos), to_bits(neg_neg));
                assert_ne!(to_bits(pos_neg), to_bits(neg_pos));
                assert_ne!(to_bits(pos_neg), to_bits(neg_neg));
                assert_ne!(to_bits(neg_pos), to_bits(neg_neg));

                Self {
                    pos_pos,
                    pos_neg,
                    neg_pos,
                    neg_neg,
                }
            }
        }

        struct Nans {
            four_nan1: Complex64,
            four_nan2: Complex64,
            nan1_four: Complex64,
            nan2_four: Complex64,
            nan1_nan1: Complex64,
            nan1_nan2: Complex64,
            nan2_nan1: Complex64,
            nan2_nan2: Complex64,
        }

        impl Nans {
            fn new() -> Self {
                let nan1 = f64::NAN;
                let nan2 = -f64::NAN;
                assert!(nan1.is_nan() && nan2.is_nan());
                assert_ne!(nan1.to_bits(), nan2.to_bits());

                let four_nan1 = c64(4.0, nan1);
                let four_nan2 = c64(4.0, nan2);
                let nan1_four = c64(nan1, 4.0);
                let nan2_four = c64(nan2, 4.0);
                let nan1_nan1 = c64(nan1, nan1);
                let nan1_nan2 = c64(nan1, nan2);
                let nan2_nan1 = c64(nan2, nan1);
                let nan2_nan2 = c64(nan2, nan2);

                assert!(four_nan1.is_nan() && !four_nan1.re.is_nan() && four_nan1.im.is_nan());
                assert!(four_nan2.is_nan() && !four_nan2.re.is_nan() && four_nan2.im.is_nan());
                assert!(nan1_four.is_nan() && nan1_four.re.is_nan() && !nan1_four.im.is_nan());
                assert!(nan2_four.is_nan() && nan2_four.re.is_nan() && !nan2_four.im.is_nan());
                assert!(nan1_nan1.is_nan() && nan1_nan1.re.is_nan() && nan1_nan1.im.is_nan());
                assert!(nan1_nan2.is_nan() && nan1_nan2.re.is_nan() && nan1_nan2.im.is_nan());
                assert!(nan2_nan1.is_nan() && nan2_nan1.re.is_nan() && nan2_nan1.im.is_nan());
                assert!(nan2_nan2.is_nan() && nan2_nan2.re.is_nan() && nan2_nan2.im.is_nan());

                assert_ne!(to_bits(four_nan1), to_bits(four_nan2));
                assert_ne!(to_bits(nan1_four), to_bits(nan2_four));

                assert_ne!(to_bits(nan1_nan1), to_bits(nan1_nan2));
                assert_ne!(to_bits(nan1_nan1), to_bits(nan2_nan1));
                assert_ne!(to_bits(nan1_nan1), to_bits(nan2_nan2));
                assert_ne!(to_bits(nan1_nan2), to_bits(nan2_nan1));
                assert_ne!(to_bits(nan1_nan2), to_bits(nan2_nan2));
                assert_ne!(to_bits(nan2_nan1), to_bits(nan2_nan2));

                Self {
                    four_nan1,
                    four_nan2,
                    nan1_four,
                    nan2_four,
                    nan1_nan1,
                    nan1_nan2,
                    nan2_nan1,
                    nan2_nan2,
                }
            }
        }

        #[test]
        fn eq_complex64_zeros() {
            let Zeros {
                pos_pos,
                pos_neg,
                neg_pos,
                neg_neg,
            } = Zeros::new();
            assert!(super::eq(pos_pos, pos_neg));
            assert!(super::eq(pos_pos, neg_pos));
            assert!(super::eq(pos_pos, neg_neg));
        }

        #[test]
        fn eq_complex64_nans() {
            let Nans {
                four_nan1,
                four_nan2,
                nan1_four,
                nan2_four,
                nan1_nan1,
                nan1_nan2,
                nan2_nan1,
                nan2_nan2,
            } = Nans::new();

            assert!(super::eq(four_nan1, four_nan2));
            assert!(super::eq(nan1_four, nan2_four));

            assert!(!super::eq(nan1_four, four_nan1));
            assert!(!super::eq(nan1_four, nan1_nan1));
            assert!(!super::eq(four_nan1, nan1_nan1));

            assert!(super::eq(nan1_nan1, nan1_nan2));
            assert!(super::eq(nan1_nan1, nan2_nan1));
            assert!(super::eq(nan1_nan1, nan2_nan2));
        }

        #[test]
        fn hash_complex64_zeros() {
            let Zeros {
                pos_pos,
                pos_neg,
                neg_pos,
                neg_neg,
            } = Zeros::new();
            assert_eq!(hash(pos_pos), hash(pos_neg));
            assert_eq!(hash(pos_pos), hash(neg_pos));
            assert_eq!(hash(pos_pos), hash(neg_neg));
        }

        #[test]
        fn hash_complex64_nans() {
            let Nans {
                four_nan1,
                four_nan2,
                nan1_four,
                nan2_four,
                nan1_nan1,
                nan1_nan2,
                nan2_nan1,
                nan2_nan2,
            } = Nans::new();

            assert_eq!(hash(four_nan1), hash(four_nan2));
            assert_eq!(hash(nan1_four), hash(nan2_four));

            assert_ne!(hash(nan1_four), hash(four_nan1));
            assert_ne!(hash(nan1_four), hash(nan1_nan1));
            assert_ne!(hash(four_nan1), hash(nan1_nan1));

            assert_eq!(hash(nan1_nan1), hash(nan1_nan2));
            assert_eq!(hash(nan1_nan1), hash(nan2_nan1));
            assert_eq!(hash(nan1_nan1), hash(nan2_nan2));
        }
    }
}
