use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
};

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

/// Hashes a HashMap by taking the XOR sum of each key value pair.
///
/// This function uses XOR, because as a commutative operation, we get the same,
/// unique XOR sum for each unique set of key value pairs, regardless of their order.
#[inline]
pub(crate) fn hash_hashmap<H: Hasher, K: Hash, V: Hash>(map: &HashMap<K, V>, state: &mut H) {
    let mut final_hash = 0;
    for element in map {
        let mut hasher = DefaultHasher::new();
        element.hash(&mut hasher);
        final_hash ^= hasher.finish();
    }

    state.write_u64(final_hash)
}

#[cfg(test)]
mod test {
    use proptest::prelude::*;

    use super::{hash_f64, hash_hashmap};
    use std::{
        collections::{hash_map::DefaultHasher, HashMap},
        hash::Hasher,
    };

    fn get_map_hash(map: &HashMap<String, u32>) -> u64 {
        let mut hasher = DefaultHasher::new();
        hash_hashmap(map, &mut hasher);
        hasher.finish()
    }

    fn get_f64_hash(float: f64) -> u64 {
        let mut hasher = DefaultHasher::new();
        hash_f64(float, &mut hasher);
        hasher.finish()
    }

    proptest! {
        #[test]
        fn test_hash_hashmap((map1, map2) in (any::<HashMap::<String, u32>>(), any::<HashMap::<String, u32>>())) {
            let hash1 = get_map_hash(&map1);
            let hash2 = get_map_hash(&map2);
            if map1 == map2 {
                assert_eq!(hash1, hash2);
            } else {
                assert_ne!(hash1, hash2);
            }
        }
    }

    #[test]
    fn test_hash_f64_zero() {
        assert_eq!(get_f64_hash(0.0), get_f64_hash(-0.0))
    }
}
