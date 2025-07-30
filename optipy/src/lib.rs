extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Field, ImplItemFn, Item, ItemEnum, ItemFn, ItemImpl, ItemMod, ItemStruct,
    PatType, Receiver, Variant, parse_macro_input,
    visit_mut::{self, VisitMut},
};

#[proc_macro_attribute]
pub fn strip_pyo3(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as Item);
    StripPyO3.visit_item_mut(&mut input);
    quote!(#input).into()
}

struct StripPyO3;

/// Visit a type by filtering its attributes, then delegating to its default implementation.
macro_rules! filter_visitor {
    ($name:ident($type:ty)) => {
        fn $name(&mut self, i: &mut $type) {
            filter_pyo3_attrs(&mut i.attrs);
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

/// Filter out `PyO3` attributes.
fn filter_pyo3_attrs(attrs: &mut Vec<Attribute>) {
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
                || id == "pymodule")
        }
    });
}

#[cfg(test)]
mod tests {
    use super::StripPyO3;
    use quote::quote;
    use syn::{Item, parse_quote, visit_mut::VisitMut};

    #[test]
    fn test_strip_struct() {
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

        StripPyO3.visit_item_mut(&mut input);
        let result = format!("{}", quote!(#input));
        assert_eq!(format!("{result}"), expected);
    }
}
