extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Field, ImplItemFn, Item, ItemEnum, ItemFn, ItemImpl, ItemMod, ItemStruct, PatType,
    Receiver, Variant, parse_macro_input,
    visit_mut::{self, VisitMut},
};

#[proc_macro_attribute]
pub fn strip_pyo3(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut stripper = StripPyO3 { only_stubs: false };

    let opt_parser = syn::meta::parser(|meta| {
        if meta.path.is_ident("only_stubs") {
            stripper.only_stubs = true;
            Ok(())
        } else {
            Err(meta.error("unsupported strip_pyo3 property"))
        }
    });
    parse_macro_input!(attr with opt_parser);

    let mut input = parse_macro_input!(item as Item);
    stripper.visit_item_mut(&mut input);
    quote!(#input).into()
}

struct StripPyO3 {
    only_stubs: bool,
}

/// Visit a type by filtering its attributes, then delegating to its default implementation.
macro_rules! filter_visitor {
    ($name:ident($type:ty)) => {
        fn $name(&mut self, i: &mut $type) {
            self.filter_pyo3_attrs(&mut i.attrs);
            visit_mut::$name(self, i);
        }
    };
}

/// Implement visitors that strip PyO3 attributes from relevant types.
impl VisitMut for StripPyO3 {
    filter_visitor!(visit_field_mut(Field));
    filter_visitor!(visit_impl_item_fn_mut(ImplItemFn));
    filter_visitor!(visit_item_enum_mut(ItemEnum));
    filter_visitor!(visit_item_fn_mut(ItemFn));
    filter_visitor!(visit_item_impl_mut(ItemImpl));
    filter_visitor!(visit_item_mod_mut(ItemMod));
    filter_visitor!(visit_item_struct_mut(ItemStruct));
    filter_visitor!(visit_pat_type_mut(PatType));
    filter_visitor!(visit_receiver_mut(Receiver));
    filter_visitor!(visit_variant_mut(Variant));
}

impl StripPyO3 {
    /// Filter out `PyO3` attributes.
    fn filter_pyo3_attrs(&self, attrs: &mut Vec<Attribute>) {
        if self.only_stubs {
            attrs.retain(|attr| !matches!(attr.path().get_ident(), Some(id) if id == "gen_stub"));
        } else {
            attrs.retain(|attr| match attr.path().get_ident() {
                None => true,
                Some(id) => {
                    !(id == "pyo3"
                        || id == "new"
                        || id == "getter"
                        || id == "setter"
                        || id == "pyclass"
                        || id == "pymethods"
                        || id == "pyfunction"
                        || id == "pymodule"
                        || id == "staticmethod"
                        || id == "classmethod"
                        || id == "classattr"
                        || id == "args"
                        || id == "gen_stub")
                }
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::StripPyO3;
    use quote::quote;
    use syn::{Item, parse_quote, visit_mut::VisitMut};

    #[test]
    fn test_strip_pyo3() {
        let mut input: Item = parse_quote! {
            mod module {
                #[pyclass]
                struct S {
                    #[pyo3(get, set)]
                    field: i32,
                }

                #[pyclass]
                enum E {
                    #[pyo3(name = "Other")]
                    Variant()
                }

                #[pymethods]
                impl S {
                    /// Construct S.
                    fn new(#[pyo3(from_py_with = foo)] field: i32) -> Self {
                        Self { field }
                    }

                    #[setter]
                    fn field(&mut self, val: i32) {
                        self.field = val;
                    }

                    #[getter]
                    fn double(&self) -> i32 {
                        self.field * 2
                    }
                }

                #[pyfunction]
                fn bar() { }

                #[pymodule]
                fn pymodule() { }
            }
        };

        let expected = format!(
            "{}",
            quote! {
            mod module {
                struct S {
                    field: i32,
                }

                enum E {
                    Variant()
                }

                impl S {
                    /// Construct S.
                    fn new(field: i32) -> Self {
                        Self { field }
                    }

                    fn field(&mut self, val: i32) {
                        self.field = val;
                    }

                    fn double(&self) -> i32 {
                        self.field * 2
                    }
                }

                fn bar() { }

                fn pymodule() { }
                }
            }
        );

        StripPyO3 { only_stubs: false }.visit_item_mut(&mut input);
        let result = format!("{}", quote!(#input));
        assert_eq!(format!("{result}"), expected);
    }

    #[test]
    fn test_strip_only_stubs() {
        let mut input: Item = parse_quote! {
            #[pymethods]
            impl S {
                fn foo(#[gen_stub(override_return_type(type_repr="int", imports=()))] field: i32) -> usize {
                    1
                }
            }
        };

        let expected = format!(
            "{}",
            quote! {
                #[pymethods]
                impl S {
                    fn foo(field: i32) -> usize {
                        1
                    }
                }
            }
        );

        StripPyO3 { only_stubs: true }.visit_item_mut(&mut input);
        let result = format!("{}", quote!(#input));
        assert_eq!(format!("{result}"), expected);
    }

    #[test]
    fn test_strip_all() {
        let mut input: Item = parse_quote! {
            #[pymethods]
            impl S {
                fn foo(#[gen_stub(override_return_type(type_repr="int", imports=()))] field: i32) -> usize {
                    1
                }
            }
        };

        let expected = format!(
            "{}",
            quote! {
                impl S {
                    fn foo(field: i32) -> usize {
                        1
                    }
                }
            }
        );

        StripPyO3 { only_stubs: false }.visit_item_mut(&mut input);
        let result = format!("{}", quote!(#input));
        assert_eq!(format!("{result}"), expected);
    }
}
