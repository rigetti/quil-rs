use crate::instruction::CalibrationSignature;

/// A [`CalibrationSet`] is a collection of calibration instructions that respect how
/// calibrations work in a Quil program.
///
/// During calibration expansion, Calibrations are matched to instructions using their unique
/// [`CalibrationSignature`]. This means only one calibration can be matched to a particular
/// instruction. While the Quil specification doesn't explicitly define the behavior of
/// signature conflicts, [`CalibrationSet`] takes the liberty of only allowing one calibration
/// per [`CalibrationSignature`].
///
/// Calibrations maintain insertion order
#[derive(Clone, Debug, PartialEq)]
pub struct CalibrationSet<T> {
    // The amount of calibrations in a program tends to be small enough that a Vec is more
    // performant than a typical set.
    // Sets also have trait bounds that `Instruction`s don't meet, which hampers utility.
    data: Vec<T>,
}

impl<T> Default for CalibrationSet<T>
where
    T: CalibrationSignature,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IntoIterator for CalibrationSet<T> {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<T> From<Vec<T>> for CalibrationSet<T>
where
    T: CalibrationSignature,
{
    fn from(data: Vec<T>) -> Self {
        let mut set = Self::with_capacity(data.len());
        for element in data {
            set.replace(element);
        }
        set
    }
}

impl<T> CalibrationSet<T>
where
    T: CalibrationSignature,
{
    /// Creates an empty [`ProgramCalibrationSet`].
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Creates a [`InnerCalibrationSet`] with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// Returns the capacity of the [`ProgramCalibrationSet`].
    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }

    /// Returns the length of the [`ProgramCalibrationSet`].
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns whether the [`ProgramCalibrationSet`] is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns an iterator of references to the values in the set.
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.data.iter()
    }

    /// Get a reference to a value that has a matching signature, if it exists.
    pub fn get(&self, signature: &<T as CalibrationSignature>::Signature<'_>) -> Option<&T> {
        if let Some(index) = self.signature_position(signature) {
            Some(&self.data[index])
        } else {
            None
        }
    }

    /// Adds a value to the set, replacing and returning an existing value with a matching
    /// [`CalibrationSignature`], if it exists.
    pub fn replace(&mut self, value: T) -> Option<T> {
        let position = self.signature_position(&value.signature());
        if let Some(index) = position {
            let replaced = std::mem::replace(&mut self.data[index], value);
            Some(replaced)
        } else {
            self.data.push(value);
            None
        }
    }

    /// Removes a value from the set. Returns whether the value was present in the set.
    pub fn remove(&mut self, signature: &<T as CalibrationSignature>::Signature<'_>) -> bool {
        if let Some(index) = self.signature_position(signature) {
            self.data.remove(index);
            true
        } else {
            false
        }
    }

    /// Returns the index of an element whose [`CalibrationSignature`] matches the given value, if one exists.
    fn signature_position(
        &self,
        signature: &<T as CalibrationSignature>::Signature<'_>,
    ) -> Option<usize> {
        for (i, element) in self.data.iter().enumerate() {
            if element.has_signature(signature) {
                return Some(i);
            }
        }

        None
        //self.data
        //    .iter()
        //    .position(move |element| { signature == element.signature() })
    }
}

impl<T> Extend<T> for CalibrationSet<T>
where
    T: CalibrationSignature,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for value in iter {
            self.replace(value);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::CalibrationSignature;

    #[derive(Clone, Debug, PartialEq)]
    struct TestCalibration {
        signature: String,
        /// Optional byte, can be used to differentiate between two TestCalibrations with the same
        /// signature.
        data: Option<u8>,
    }

    impl TestCalibration {
        fn new(signature: &str, data: Option<u8>) -> Self {
            Self {
                signature: signature.to_string(),
                data,
            }
        }
    }

    impl CalibrationSignature for TestCalibration {
        type Signature<'a> = &'a str;

        fn signature(&self) -> Self::Signature<'_> {
            self.signature.as_str()
        }

        fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
            self.signature == *signature
        }
    }

    #[test]
    fn test_replace() {
        let mut set = CalibrationSet::new();
        let calib = TestCalibration::new("Signature", None);
        set.replace(calib.clone());
        assert_eq!(set.len(), 1);
        assert!(set.iter().all(|item| item == &calib));

        let calib2 = TestCalibration::new("Signature", Some(42));
        let replaced = set
            .replace(calib2.clone())
            .expect("The original calibration should have been replaced.");
        assert_eq!(replaced, calib);
        assert_eq!(set.len(), 1);
        assert!(set.iter().all(|item| item == &calib2));
    }

    #[test]
    fn test_remove_signature() {
        let mut set = CalibrationSet::new();
        let calib = TestCalibration::new("Original", None);
        set.replace(calib.clone());
        assert!(set.remove(&calib.signature()));
        assert!(set.is_empty());
    }

    #[test]
    fn test_order_sensitive_equality() {
        let mut set1 = CalibrationSet::new();
        let mut set2 = CalibrationSet::new();
        let calib1 = TestCalibration::new("1", None);
        let calib2 = TestCalibration::new("2", None);
        set1.extend(vec![calib1.clone(), calib2.clone()]);
        set2.extend(vec![calib2, calib1]); // Reverse order
        assert_ne!(set1, set2)
    }

    #[test]
    fn test_maintains_insert_order() {
        let calibs = vec![
            TestCalibration::new("1", None),
            TestCalibration::new("2", None),
            TestCalibration::new("3", None),
        ];
        let set = CalibrationSet::from(calibs.clone());
        let calibs_from_set: Vec<TestCalibration> = set.into_iter().collect();
        assert_eq!(calibs_from_set, calibs);
    }

    #[test]
    fn test_with_capacity() {
        let capacity = 10;
        let set: CalibrationSet<TestCalibration> = CalibrationSet::with_capacity(capacity);
        assert_eq!(set.capacity(), capacity);
    }
}
