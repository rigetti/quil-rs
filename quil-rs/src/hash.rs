use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

/// Hash value helper: turn a hashable thing into a u64.
#[inline]
pub(crate) fn hash_to_u64<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
/// Hashes a f64 using its u64 representation.
///
/// Notes:
/// * This function hashes +0.0 and -0.0 to the same value.
/// * The [documentation](https://doc.rust-lang.org/std/primitive.f64.html#method.to_bits) claims
///   that this is generally portable in practice.
#[inline]
pub(crate) fn hash_f64<H: Hasher>(value: f64, state: &mut H) {
    // +0.0 and -0.0 have different bits, so we use the bit form of +0.0 for both, that way
    // hash(+0.0) == hash(-0.0)
    if value == 0.0f64 {
        0.0f64.to_bits().hash(state)
    } else {
        value.to_bits().hash(state)
    }
}

#[cfg(test)]
mod test {
    use super::hash_f64;
    use std::{collections::hash_map::DefaultHasher, hash::Hasher};

    fn get_f64_hash(float: f64) -> u64 {
        let mut hasher = DefaultHasher::new();
        hash_f64(float, &mut hasher);
        hasher.finish()
    }

    #[test]
    fn test_hash_f64_zero() {
        assert_eq!(get_f64_hash(0.0), get_f64_hash(-0.0))
    }
}
