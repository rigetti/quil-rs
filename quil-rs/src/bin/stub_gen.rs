//! This binary is used to generate Python stub files (type hints) for the `quil` package.
//! For more information on why this exists as a separate binary rather than a build script,
//! see the [`pyo3-stub-gen`][] documentation.
//!
//! [`pyo3-stub-gen`]: https://github.com/Jij-Inc/pyo3-stub-gen

#[cfg(feature = "stubs")]
/// Provide support for sorting `.pyi` stubs.
///
/// [`pyo3-stub-gen`] doesn't keep its output in a consistent order.  Thankfully the order is
/// deterministic if the program is deterministic, but due to the use of the [`inventory`][] crate,
/// it's not deterministic if the code changes.  This module thus sorts the components of stubs that
/// are not already in a fixed order.
///
/// [`pyo3-stub-gen`]: https://github.com/Jij-Inc/pyo3-stub-gen
mod sort_stubs {
    use std::{
        any::TypeId,
        cmp::Ordering,
        collections::{BTreeMap, BTreeSet, HashSet},
        path::PathBuf,
    };

    use indexmap::IndexMap;
    use itertools::Itertools as _;
    use pyo3_stub_gen::{
        generate::{
            Arg, ClassDef, EnumDef, FunctionDef, MemberDef, MethodDef, MethodType, Module,
            VariableDef,
        },
        type_info::{DeprecatedInfo, IgnoreTarget, SignatureArg},
        StubInfo, TypeInfo,
    };

    /// A trait that's equivalent to [`Ord`] but not semantically meaningful, used for putting
    /// [`pyo3_stub_gen`] types in a consistent order.
    trait ArbitraryOrd {
        /// Analogous to [`Ord::cmp`]
        #[must_use]
        fn cmp(&self, other: &Self) -> Ordering;
    }

    /// Wrap references to an [`Ord`] type as an [`ArbitraryOrd`] type.
    struct Arbitrary<'a, T>(&'a T);

    impl<T: ArbitraryOrd> PartialEq for Arbitrary<'_, T> {
        fn eq(&self, other: &Self) -> bool {
            self.cmp(other) == Ordering::Equal
        }
    }

    impl<T: ArbitraryOrd> Eq for Arbitrary<'_, T> {}

    impl<T: ArbitraryOrd> PartialOrd for Arbitrary<'_, T> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl<T: ArbitraryOrd> Ord for Arbitrary<'_, T> {
        fn cmp(&self, other: &Self) -> Ordering {
            self.0.cmp(other.0)
        }
    }

    macro_rules! arbitrary_ord_structs {
        ($(
            $struct:ident { $($field:ident),* $(,)? };
        )*) => {
            $(
                #[automatically_derived]
                impl ArbitraryOrd for $struct {
                    fn cmp(&self, other: &Self) -> Ordering {
                        // This guarantees us exhaustiveness
                        let $struct { $($field),* } = self;

                        // Return the first non-Equal result when comparing field pairs,
                        // or return Equal if all field pairs compare Equal.
                        let result = Ordering::Equal;
                        $(
                            let result = $field.cmp(&other.$field);
                            if result != Ordering::Equal {
                                return result;
                            }
                        )*
                        return result;
                    }
                }
            )*
        }
    }

    impl<T: Ord> ArbitraryOrd for HashSet<T> {
        fn cmp(&self, other: &Self) -> Ordering {
            let self_sorted: Vec<_> = self.iter().sorted().collect();
            let other_sorted: Vec<_> = other.iter().sorted().collect();
            self_sorted.cmp(&other_sorted)
        }
    }

    impl<T: ArbitraryOrd> ArbitraryOrd for Option<T> {
        fn cmp(&self, other: &Self) -> Ordering {
            match (self, other) {
                (None, None) => Ordering::Equal,
                (None, Some(_)) => Ordering::Less,
                (Some(_), None) => Ordering::Greater,
                (Some(left), Some(right)) => left.cmp(right),
            }
        }
    }

    impl<T: ArbitraryOrd> ArbitraryOrd for (T, T) {
        fn cmp(&self, other: &Self) -> Ordering {
            match self.0.cmp(&other.0) {
                Ordering::Equal => self.1.cmp(&other.1),
                not_equal => not_equal,
            }
        }
    }

    impl<T: ArbitraryOrd> ArbitraryOrd for Vec<T> {
        fn cmp<'a>(&'a self, other: &'a Self) -> Ordering {
            let sort = |vec: &'a Self| -> Vec<_> {
                vec.iter()
                    .sorted_by(|l, r| l.cmp(r))
                    .map(Arbitrary)
                    .collect()
            };

            sort(self).cmp(&sort(other))
        }
    }

    impl<K: Ord, V: ArbitraryOrd> ArbitraryOrd for IndexMap<K, V> {
        fn cmp<'a>(&'a self, other: &'a Self) -> Ordering {
            let sort = |map: &'a Self| -> Vec<_> {
                map.iter()
                    .sorted_by(|(lk, _), (rk, _)| lk.cmp(rk))
                    .map(|(k, v)| (k, Arbitrary(v)))
                    .collect()
            };
            sort(self).cmp(&sort(other))
        }
    }

    impl ArbitraryOrd for IgnoreTarget {
        fn cmp(&self, other: &Self) -> Ordering {
            match (self, other) {
                (IgnoreTarget::All, IgnoreTarget::All) => Ordering::Equal,
                (IgnoreTarget::All, IgnoreTarget::Specified(_)) => Ordering::Less,
                (IgnoreTarget::Specified(_), IgnoreTarget::All) => Ordering::Greater,
                (IgnoreTarget::Specified(left), IgnoreTarget::Specified(right)) => left.cmp(right),
            }
        }
    }

    impl ArbitraryOrd for SignatureArg {
        fn cmp(&self, other: &Self) -> Ordering {
            match (self, other) {
                (SignatureArg::Ident, SignatureArg::Ident) => Ordering::Equal,
                (SignatureArg::Ident, _) => Ordering::Less,
                (_, SignatureArg::Ident) => Ordering::Greater,

                (
                    SignatureArg::Assign { default: left },
                    SignatureArg::Assign { default: right },
                ) => left().cmp(&right()),
                (SignatureArg::Assign { default: _ }, _) => Ordering::Less,
                (_, SignatureArg::Assign { default: _ }) => Ordering::Greater,

                (SignatureArg::Star, SignatureArg::Star) => Ordering::Equal,
                (SignatureArg::Star, _) => Ordering::Less,
                (_, SignatureArg::Star) => Ordering::Greater,

                (SignatureArg::Args, SignatureArg::Args) => Ordering::Equal,
                (SignatureArg::Args, _) => Ordering::Less,
                (_, SignatureArg::Args) => Ordering::Greater,

                (SignatureArg::Keywords, SignatureArg::Keywords) => Ordering::Equal,
            }
        }
    }

    impl ArbitraryOrd for MethodType {
        fn cmp(&self, other: &Self) -> Ordering {
            match (self, other) {
                (MethodType::Instance, MethodType::Instance) => Ordering::Equal,
                (MethodType::Instance, _) => Ordering::Less,
                (_, MethodType::Instance) => Ordering::Equal,

                (MethodType::Static, MethodType::Static) => Ordering::Equal,
                (MethodType::Static, _) => Ordering::Less,
                (_, MethodType::Static) => Ordering::Equal,

                (MethodType::Class, MethodType::Class) => Ordering::Equal,
                (MethodType::Class, _) => Ordering::Less,
                (_, MethodType::Class) => Ordering::Equal,

                (MethodType::New, MethodType::New) => Ordering::Equal,
            }
        }
    }

    arbitrary_ord_structs! {
        TypeInfo { name, import };
        MemberDef { name, r#type, doc, default, deprecated };
        MethodDef { name, args, r#return, doc, r#type, is_async, deprecated, type_ignored };
        DeprecatedInfo { since, note };
        Arg { name, r#type, signature };
        ClassDef { name, doc, attrs, getter_setters, methods, bases, classes, match_args };
    }

    // Inside the sorting functions, we check *every field* to check if we should sort it.  In order
    // to make sure we've covered everything, and to get the compiler to yell at us if we've missed
    // anything or there are any changes, we aggressively over-annotate all the types.  This allows
    // seeing immediately where sorting bottoms out.

    fn sort_class(class: &mut ClassDef) {
        let ClassDef {
            name,
            doc,
            attrs: _, // Regardless of the type of the field, we can't reorder attributes
            getter_setters,
            methods,  // A map from names to overload sets; overloads can't be reordered
            bases: _, // Regardless of the type of the field, we can't reorder base classes
            classes,
            match_args: _, // Regardless of the type of the field, we can't reorder match args
        } = class;

        let _: &str = name;
        let _: &str = doc;

        // [`MemberDef`]s are atomic and don't have contents that need to be sorted.
        <IndexMap<String, (Option<MemberDef>, Option<MemberDef>)>>::sort_by_key(
            getter_setters,
            |k, _| k.clone(),
        );

        // [`MethodDef`]s are atomic and don't have contents that need to be sorted.  We
        // have to `clone` in the sorting function because [`IndexMap::sort_by_key`]'s
        // lifetimes are too restrictive.
        <IndexMap<String, Vec<MethodDef>>>::sort_by_key(methods, |k, _| k.clone());

        // Finally, [`ClassDef`]s both need to be sorted internally and need to be produced in
        // sorted order.
        <[ClassDef]>::iter_mut(classes).for_each(sort_class);
        <[ClassDef]>::sort_by(classes, ArbitraryOrd::cmp);
    }

    fn sort_enum(r#enum: &mut EnumDef) {
        let EnumDef {
            name,
            doc,
            variants: _, // Regardless of the type of the field, we can't reorder the variants
            methods,
            attrs: _, // Regardless of the type of the field, we can't reorder the variants attributes
            getters,
            setters,
        } = r#enum;

        // Names (being strings) don't any adjustment.
        let _: &str = name;
        let _: &str = doc;

        // [`MethodDef`]s and [`MemberDef`]s are atomic and don't have contents that need to be
        // sorted.
        <[MethodDef]>::sort_by(methods, ArbitraryOrd::cmp);
        <[MemberDef]>::sort_by(getters, ArbitraryOrd::cmp);
        <[MemberDef]>::sort_by(setters, ArbitraryOrd::cmp);
    }

    fn sort_module(module: &mut Module) {
        let Module {
            doc,
            class,
            enum_,
            function,
            variables,
            name,
            default_module_name,
            submodules,
        } = module;

        // Doc strings don't any adjustment.
        let _: &str = doc;

        // `function` is an ordered map from function names to overload sets; since overload sets
        // themselves can't be reordered, there's nothing else to do.
        let _: &BTreeMap<&str, Vec<FunctionDef>> = function;

        // [`VariableDef`]s are atomic; they don't themselves have internal structure that needs to
        // be reordered.
        let _: &BTreeMap<&str, VariableDef> = variables;

        // Names (being strings) don't need any adjustment.
        let _: &String = name;
        let _: &String = default_module_name;

        // The submodules are stored as an ordered set of names, so we don't need to do any more
        // sorting.
        let _: &BTreeSet<String> = submodules;

        // The [`class`]es and [`enum_`]s are sorted because they're kept in [`BTreeMap`]s, but we
        // need to sort the individual [`ClassDef`]s and [`EnumDef`]s internally.  You might be,
        // rightly, concerned about the fact that the ordering on [`TypeId`]s is arbitrary; however,
        // [`pyo3_stub_gen`] sorts the [`ClassDef`]s and [`EnumDef`]s by their names before writing
        // them out.
        <BTreeMap<TypeId, ClassDef>>::values_mut(class).for_each(sort_class);
        <BTreeMap<TypeId, EnumDef>>::values_mut(enum_).for_each(sort_enum);
    }

    /// Destructively sort all the unsorted components of a [`StubInfo`].
    pub fn sort(stub: &mut StubInfo) {
        let StubInfo {
            modules,
            python_root,
        } = stub;

        // The Python root is fixed and doesn't need any adjustment.
        let _: &PathBuf = python_root;

        // The `modules` are sorted because they're in a `BTreeMap`, but we need to sort their
        // contents.
        <BTreeMap<String, Module>>::values_mut(modules).for_each(sort_module);
    }
}

#[cfg(feature = "stubs")]
fn main() -> pyo3_stub_gen::Result<()> {
    let mut stub = quil_rs::quilpy::stub_info()?;
    sort_stubs::sort(&mut stub);
    stub.generate()?;
    Ok(())
}

#[cfg(not(feature = "stubs"))]
fn main() {
    eprintln!("Executing this binary only makes sense with the --stubs feature enabled.");
}
