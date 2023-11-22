use rigetti_pyo3::create_init_submodule;

pub mod identifier;

create_init_submodule! {
    submodules : ["identifier": identifier::init_submodule],
}
