use crate::instruction::CalibrationSignature;

/// A [`ProgramCalibrationSet`] is a collection of calibration instructions that respect how
/// calibrations work in a Quil program.
///
/// During calibration expansion, Calibrations are matched to instructions using their unique
/// [`CalibrationSignature`]. This means only one calibration can be matched to a particular
/// instruction. While the Quil specification doesn't explicitly define this behavior, the
/// [`ProgramCalibrationSet`] takes the liberty of only allowing one calibration per
/// [`CalibrationSignature`].
///
/// In addition, calibration instructions are global. That is, their order or location in a
/// program make no semantic difference. This means two program [`ProgramCalibrationSet`]s
/// should be considered equal if they contain the same set of calibrations, regardless of
/// order.
///
/// As a matter of convenience, the insertion order of calibrations is maintained. This
/// allows for the same set of calibrations to be deterministically serialized to Quil.
#[derive(Clone, Debug)]
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
        let mut set = Self::new();
        for element in data {
            set.replace_signature(element);
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

    /// Adds a value to the set, replacing and returning an existing value with the same
    /// [`CalibrationSignature`], if it exists.
    pub fn replace_signature(&mut self, value: T) -> Option<T> {
        if let Some(index) = self.signature_position(value.signature()) {
            let replaced = std::mem::replace(&mut self.data[index], value);
            Some(replaced)
        } else {
            self.data.push(value);
            None
        }
    }

    /// Removes a value from the set. Returns whether the value was present in the set.
    pub fn remove_signature(&mut self, signature: <T as CalibrationSignature>::Signature) -> bool {
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
        signature: <T as CalibrationSignature>::Signature,
    ) -> Option<usize> {
        self.data
            .iter()
            .position(|element| element.signature() == signature)
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
            self.replace_signature(value);
        }
    }
}

impl<T> PartialEq for CalibrationSet<T>
where
    T: CalibrationSignature + PartialEq,
    <T as CalibrationSignature>::Signature: std::hash::Hash + Eq,
{
    fn eq(&self, other: &Self) -> bool {
        let self_map: std::collections::HashMap<T::Signature, &T> = self
            .data
            .iter()
            .map(|element| (element.signature(), element))
            .collect();
        let other_map: std::collections::HashMap<T::Signature, &T> = other
            .data
            .iter()
            .map(|element| (element.signature(), element))
            .collect();
        self_map == other_map
    }
}
