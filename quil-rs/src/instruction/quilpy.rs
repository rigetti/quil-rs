
#[macro_export]
macro_rules! pickleable_new {
    // Default implementation: just list the fields and types, and this will do the rest.
    (
        impl $name:ident {
            $(#[$meta:meta])*
            $pub:vis fn $new:ident( $($field:ident: $field_type:ty$(,)?)*);
        }
    ) => {
        pickleable_new! {
            impl $name {
                $(#[$meta])*
                $pub fn $new($($field: $field_type,)*) -> Self {
                    Self {
                        $($field,)*
                    }
                }
            }
        }
    };

    // If __new__ needs actual logic, you can supply a body. 
    (
        impl $name:ident {
            $(#[$meta:meta])*
            $pub:vis fn $new:ident( $($field:ident: $field_type:ty$(,)?)*) -> $Self:ty {
                $($body:tt)+
            }
        }
    ) => {
        #[pymethods]
        impl $name {
            #[new]
            $(#[$meta])*
            $pub fn $new($($field: $field_type,)*) -> $Self {
                $($body)+
            }

            fn __getnewargs__(&self) -> ($($field_type,)*) { 
                (
                    $(self.$field.clone(),)*
                )
            }
        }
    };
}
