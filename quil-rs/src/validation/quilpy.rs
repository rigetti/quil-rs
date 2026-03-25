use rigetti_pyo3::create_init_submodule;

mod identifier {
    use crate::quilpy::errors;
    use crate::validation::identifier::{validate_identifier, validate_user_identifier};
    use rigetti_pyo3::create_init_submodule;

    create_init_submodule! {
        errors: [ errors::IdentifierValidationError ],
        funcs: [ validate_identifier, validate_user_identifier ],
    }
}

create_init_submodule! {
    submodules: [ "identifier": identifier::init_submodule ],
}
