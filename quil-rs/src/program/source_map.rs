use super::InstructionIndex;

/// A SourceMap provides information necessary to understand which parts of a target
/// were derived from which parts of a source artifact, in such a way that they can be
/// mapped in either direction.
///
/// The behavior of such mappings depends on the implementations of the generic `Index` types,
/// but this may be a many-to-many mapping, where one element of the source is mapped (contributes)
/// to zero or many elements of the target, and vice versa.
///
/// This is also intended to be mergeable in a chain, such that the combined result of a series
/// of transformations can be expressed within a single source mapping.
#[derive(Clone, Debug, PartialEq)]
pub struct SourceMap<SourceIndex, TargetIndex> {
    pub(crate) entries: Vec<SourceMapEntry<SourceIndex, TargetIndex>>,
}

impl<SourceIndex, TargetIndex> SourceMap<SourceIndex, TargetIndex> {
    /// Return all source ranges in the source map which were used to generate the target range.
    ///
    /// This is `O(n)` where `n` is the number of entries in the map.
    pub fn list_sources<QueryIndex>(&self, target_index: &QueryIndex) -> Vec<&SourceIndex>
    where
        TargetIndex: SourceMapIndexable<QueryIndex>,
    {
        self.entries
            .iter()
            .filter_map(|entry| {
                if entry.target_location().intersects(target_index) {
                    Some(entry.source_location())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Return all target ranges in the source map which were used to generate the source range.
    ///
    /// This is `O(n)` where `n` is the number of entries in the map.
    pub fn list_targets<QueryIndex>(&self, source_index: &QueryIndex) -> Vec<&TargetIndex>
    where
        SourceIndex: SourceMapIndexable<QueryIndex>,
    {
        self.entries
            .iter()
            .filter_map(|entry| {
                if entry.source_location().intersects(source_index) {
                    Some(entry.target_location())
                } else {
                    None
                }
            })
            .collect()
    }
}

impl<SourceIndex, TargetIndex> Default for SourceMap<SourceIndex, TargetIndex> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<SourceIndex, TargetIndex> SourceMap<SourceIndex, TargetIndex> {
    pub fn entries(&self) -> &[SourceMapEntry<SourceIndex, TargetIndex>] {
        &self.entries
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SourceMapEntry<SourceIndex, TargetIndex> {
    /// The locator within the source artifact
    pub(crate) source_location: SourceIndex,

    /// The locator within the target artifact
    pub(crate) target_location: TargetIndex,
}

impl<SourceIndex, TargetIndex> SourceMapEntry<SourceIndex, TargetIndex> {
    pub fn source_location(&self) -> &SourceIndex {
        &self.source_location
    }

    pub fn target_location(&self) -> &TargetIndex {
        &self.target_location
    }
}

/// A trait for types which can be used as lookup indices in a `SourceMap.`
pub trait SourceMapIndexable<Index> {
    /// Return `true` if a source or target index intersects `other`.
    fn intersects(&self, other: &Index) -> bool;
}

impl SourceMapIndexable<InstructionIndex> for InstructionIndex {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        self == other
    }
}

/// Creates Python wrappers for `SourceMap<$srcT, $tgtT>` and `SourceMapEntry<$T, $U>`,
/// then implements `pymethods` for them to forward to the inner type.
/// This is necessary because a `#[pyclass]` can't have generic parameters,
/// and concrete implementations must be generated explicitly.
///
/// The syntax of the macro is meant to look like a regular newtype declaration
/// for a wrappers around the above types, except you can't specify the `SourceMapEntry` types
/// (they're forced to match the types for the `SourceMap` itself).
/// The structs automatically get `#[pyclass]` and `#[derive(Clone, Debug, PartialEq)]`,
/// but you can add additional annotations as desired, e.g. to change the Python name and module:
///
/// ```
/// py_source_map! {
///     #[pyo3(name = "CalibrationExpansionSourceMap", module = "quil.program", frozen)]
///     PyCalibrationExpansionSourceMap(SourceMap<InstructionIndex, MaybeCalibrationExpansion>);
///
///     #[pyo3(name = "CalibrationExpansionSourceMapEntry", module = "quil.program", frozen)]
///     PyCalibrationExpansionSourceMapEntry(SourceMapEntry<...>);
/// }
/// ```
#[macro_export]
macro_rules! py_source_map {
    (
        $(#[$meta_map: meta])*
        $mapvis:vis struct $mapT: ident(SourceMap<$srcT: ty, $tgtT: ty>);

        $(#[$meta_entry: meta])*
        $entryvis:vis struct $entryT: ident(SourceMapEntry<...>);
    ) => {
        #[derive(Clone, Debug, PartialEq)]
        #[pyclass]
        $(#[$meta_map])*
        $mapvis struct $mapT(pub SourceMap<$srcT, $tgtT>);

        #[derive(Clone, Debug, PartialEq)]
        #[pyclass]
        $(#[$meta_entry])*
        $entryvis struct $entryT(pub SourceMapEntry<$srcT, $tgtT>);

        #[pymethods]
        impl $entryT {
            pub fn source_location(&self) -> $srcT {
                *self.0.source_location()
            }

            pub fn target_location(&self) -> $tgtT {
                self.0.target_location().clone()
            }
        }

        #[pymethods]
        impl $mapT {
            fn entries(&self) -> Vec<$entryT> {
                self.0
                    .entries()
                    .iter()
                    .map(|entry| $entryT(entry.clone()))
                    .collect()
            }

            /// Given an instruction index within the resulting expansion, return the locations in the source
            /// which were expanded to generate that instruction.
            ///
            /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
            fn list_sources_for_target_index(&self, target_index: $srcT) -> Vec<&$srcT> {
                self.0.list_sources(&target_index)
            }

            /// Given a particular calibration (`DEFCAL` or `DEFCAL MEASURE`), return the locations in the source
            /// program which were expanded using that calibration.
            ///
            /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
            fn list_sources_for_calibration_used(
                &self,
                calibration_used: CalibrationSource,
            ) -> Vec<&$srcT> {
                self.0.list_sources(&calibration_used)
            }

            /// Given a source index, return information about its expansion.
            ///
            /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
            fn list_targets_for_source_index(&self, source_index: $srcT) -> Vec<$tgtT> {
                self.0
                    .list_targets(&source_index)
                    .into_iter()
                    .map(|ext| ext.clone())
                    .collect()
            }
        }
    };
}
