// use rigetti_pyo3::py_wrap_union_enum;

pub mod arithmetic;
pub mod expression;
pub mod gate;
pub mod memory_reference;

// TODO: This _may_ work after every associated type has python bindings
// py_wrap_union_enum! {
//     PyInstruction(Instruction) as "Instruction" {
//         halt: Halt,
//         nop: Nop,
//         arithmetic: Arithmetic => PyArithmetic,
//         gate: Gate => PyGate
//     }
// }
