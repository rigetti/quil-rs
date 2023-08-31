use crate::{
    expression::Expression,
    imag,
    instruction::{write_expression_parameter_string, write_parameter_string, write_qubits, Qubit},
    quil::{write_join_quil, Quil},
    real,
    validation::identifier::{
        validate_identifier, validate_user_identifier, IdentifierValidationError,
    },
};
use ndarray::{array, linalg::kron, Array2};
use num_complex::Complex64;
use once_cell::sync::Lazy;
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

/// A struct encapsulating all the properties of a Quil Quantum Gate.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Gate {
    pub name: String,
    pub parameters: Vec<Expression>,
    pub qubits: Vec<Qubit>,
    pub modifiers: Vec<GateModifier>,
}

/// An enum of all the possible modifiers on a quil [`Gate`]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GateModifier {
    /// The `CONTROLLED` modifier makes the gate take an extra [`Qubit`] parameter as a control
    /// qubit.
    Controlled,
    /// The `DAGGER` modifier does a complex-conjugate transpose on the [`Gate`].
    Dagger,
    /// The `FORKED` modifier allows an alternate set of parameters to be used based on the state
    /// of a qubit.
    Forked,
}

#[derive(Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum GateError {
    #[error("invalid name: {0}")]
    InvalidIdentifier(#[from] IdentifierValidationError),

    #[error("a gate must operate on 1 or more qubits")]
    EmptyQubits,

    #[error("expected {expected} parameters, but got {actual}")]
    ForkedParameterLength { expected: usize, actual: usize },

    #[error("expected the number of Pauli term arguments, {actual}, to match the length of the Pauli word, {expected}")]
    PauliTermArgumentLength { expected: usize, actual: usize },

    #[error("the Pauli term arguments {mismatches:?}, are not in the defined argument list: {expected_arguments:?}")]
    PauliSumArgumentMismatch {
        mismatches: Vec<String>,
        expected_arguments: Vec<String>,
    },

    #[error("unknown gate `{name}` to turn into {} matrix ",  if *parameterized { "parameterized" } else { "constant" })]
    UndefinedGate { name: String, parameterized: bool },

    #[error("expected {expected} parameters, was given {actual}")]
    MatrixArgumentLength { expected: usize, actual: usize },

    #[error(
        "cannot produce a matrix for a gate `{name}` with non-constant parameters {parameters:?}"
    )]
    MatrixNonConstantParams {
        name: String,
        parameters: Vec<Expression>,
    },

    #[error("cannot produce a matrix for gate `{name}` with variable qubit {qubit}", qubit=.qubit.to_quil_or_debug())]
    MatrixVariableQubit { name: String, qubit: Qubit },

    #[error("forked gate `{name}` has an odd number of parameters: {parameters:?}")]
    ForkedGateOddNumParams {
        name: String,
        parameters: Vec<Expression>,
    },

    #[error("cannot produce a matrix for a gate `{name}` with unresolved qubit placeholders")]
    UnresolvedQubitPlaceholder { name: String },
}

/// Matrix version of a gate.
pub type Matrix = Array2<Complex64>;

impl Gate {
    /// Build a new gate
    ///
    /// # Errors
    ///
    /// Returns an error if the given name isn't a valid Quil identifier or if no qubits are given.
    pub fn new(
        name: &str,
        parameters: Vec<Expression>,
        qubits: Vec<Qubit>,
        modifiers: Vec<GateModifier>,
    ) -> Result<Self, GateError> {
        if qubits.is_empty() {
            return Err(GateError::EmptyQubits);
        }

        validate_identifier(name).map_err(GateError::InvalidIdentifier)?;

        Ok(Self {
            name: name.to_string(),
            parameters,
            qubits,
            modifiers,
        })
    }

    /// Apply a DAGGER modifier to the gate
    pub fn dagger(mut self) -> Self {
        self.modifiers.insert(0, GateModifier::Dagger);
        self
    }

    /// Apply a CONTROLLED modifier to the gate
    pub fn controlled(mut self, control_qubit: Qubit) -> Self {
        self.qubits.insert(0, control_qubit);
        self.modifiers.insert(0, GateModifier::Controlled);
        self
    }

    /// Apply a FORKED modifier to the gate
    ///
    /// # Errors
    ///
    /// Returns an error if the number of provided alternate parameters don't
    /// equal the number of existing parameters.
    pub fn forked(
        mut self,
        fork_qubit: Qubit,
        alt_params: Vec<Expression>,
    ) -> Result<Self, GateError> {
        if alt_params.len() != self.parameters.len() {
            return Err(GateError::ForkedParameterLength {
                expected: self.parameters.len(),
                actual: alt_params.len(),
            });
        }
        self.modifiers.insert(0, GateModifier::Forked);
        self.qubits.insert(0, fork_qubit);
        self.parameters.extend(alt_params);
        Ok(self)
    }

    /// Lift a Gate to the full `n_qubits`-qubit Hilbert space.
    ///
    /// # Errors
    ///
    /// Returns an error if any of the parameters of this gate are non-constant, if any of the
    /// qubits are variable, if the name of this gate is unknown, or if there are an unexpected
    /// number of parameters.
    pub fn to_unitary(&mut self, n_qubits: u64) -> Result<Matrix, GateError> {
        let qubits = self
            .qubits
            .iter()
            .map(|q| match q {
                Qubit::Variable(_) => Err(GateError::MatrixVariableQubit {
                    name: self.name.clone(),
                    qubit: q.clone(),
                }),
                Qubit::Placeholder(_) => Err(GateError::UnresolvedQubitPlaceholder {
                    name: self.name.clone(),
                }),
                Qubit::Fixed(i) => Ok(*i),
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(lifted_gate_matrix(&gate_matrix(self)?, &qubits, n_qubits))
    }
}

/// Lift a unitary matrix to act on the specified qubits in a full `n_qubits`-qubit Hilbert
/// space.
///
/// For 1-qubit gates, this is easy and can be achieved with appropriate kronning of identity
/// matrices. For 2-qubit gates acting on adjacent qubit indices, it is also easy. However, for a
/// multiqubit gate acting on non-adjactent qubit indices, we must first apply a permutation matrix
/// to make the qubits adjacent and then apply the inverse permutation.
fn lifted_gate_matrix(matrix: &Matrix, qubits: &[u64], n_qubits: u64) -> Matrix {
    let (perm, start) = permutation_arbitrary(qubits, n_qubits);
    let v = qubit_adjacent_lifted_gate(start, matrix, n_qubits);
    perm.t().mapv(|c| c.conj()).dot(&v.dot(&perm))
}

/// Recursively handle a gate, with all modifiers.
///
/// The main source of complexity is in handling FORKED gates. Given a gate with modifiers, such as
/// `FORKED CONTROLLED FORKED RX(a,b,c,d) 0 1 2 3`, we get a tree, as in
///
/// ```text
///
///               FORKED CONTROLLED FORKED RX(a,b,c,d) 0 1 2 3
///                 /                                      \
///    CONTROLLED FORKED RX(a,b) 1 2 3       CONTROLLED FORKED RX(c,d) 1 2 3
///                |                                        |
///         FORKED RX(a,b) 2 3                      FORKED RX(c,d) 2 3
///          /          \                            /          \
///      RX(a) 3      RX(b) 3                    RX(c) 3      RX(d) 3
/// ```
fn gate_matrix(gate: &mut Gate) -> Result<Matrix, GateError> {
    static ZERO: Lazy<Matrix> =
        Lazy::new(|| array![[real!(1.0), real!(0.0)], [real!(0.0), real!(0.0)]]);
    static ONE: Lazy<Matrix> =
        Lazy::new(|| array![[real!(0.0), real!(0.0)], [real!(0.0), real!(1.0)]]);
    if let Some(modifier) = gate.modifiers.pop() {
        match modifier {
            GateModifier::Controlled => {
                gate.qubits = gate.qubits[1..].to_vec();
                let matrix = gate_matrix(gate)?;
                Ok(kron(&ZERO, &Array2::eye(matrix.shape()[0])) + kron(&ONE, &matrix))
            }
            GateModifier::Dagger => gate_matrix(gate).map(|g| g.t().mapv(|c| c.conj())),
            GateModifier::Forked => {
                let param_index = gate.parameters.len();
                if param_index & 1 != 0 {
                    Err(GateError::ForkedGateOddNumParams {
                        name: gate.name.clone(),
                        parameters: gate.parameters.clone(),
                    })
                } else {
                    // Some mutability dancing to keep the borrow checker happy
                    gate.qubits = gate.qubits[1..].to_vec();
                    let (p0, p1) = gate.parameters[..].split_at(param_index / 2);
                    let mut child0 = gate.clone();
                    child0.parameters = p0.to_vec();
                    let mat0 = gate_matrix(&mut child0)?;
                    gate.parameters = p1.to_vec();
                    let mat1 = gate_matrix(gate)?;
                    Ok(kron(&ZERO, &mat0) + kron(&ONE, &mat1))
                }
            }
        }
    } else if gate.parameters.is_empty() {
        CONSTANT_GATE_MATRICES
            .get(&gate.name)
            .cloned()
            .ok_or_else(|| GateError::UndefinedGate {
                name: gate.name.clone(),
                parameterized: false,
            })
    } else {
        match gate.parameters.len() {
            1 => {
                if let Expression::Number(x) = gate.parameters[0].clone().into_simplified() {
                    PARAMETERIZED_GATE_MATRICES
                        .get(&gate.name)
                        .map(|f| f(x))
                        .ok_or_else(|| GateError::UndefinedGate {
                            name: gate.name.clone(),
                            parameterized: true,
                        })
                } else {
                    Err(GateError::MatrixNonConstantParams {
                        name: gate.name.clone(),
                        parameters: gate.parameters.clone(),
                    })
                }
            }
            actual => Err(GateError::MatrixArgumentLength {
                expected: 1,
                actual,
            }),
        }
    }
}

/// Generate the permutation matrix that permutes an arbitrary number of single-particle Hilbert
/// spaces into adjacent positions.
///
///
/// Transposes the qubit indices in the order they are passed to a contiguous region in the
/// complete Hilbert space, in increasing qubit index order (preserving the order they are passed
/// in).
///
/// Gates are usually defined as `GATE 0 1 2`, with such an argument ordering dictating the layout
/// of the matrix corresponding to GATE. If such an instruction is given, actual qubits (0, 1, 2)
/// need to be swapped into the positions (2, 1, 0), because the lifting operation taking the 8 x 8
/// matrix of GATE is done in the little-endian (reverse) addressed qubit space.
///
/// For example, suppose I have a Quil command CCNOT 20 15 10. The median of the qubit indices is
/// 15 - hence, we permute qubits [20, 15, 10] into the final map [16, 15, 14] to minimize the
/// number of swaps needed, and so we can directly operate with the final CCNOT, when lifted from
/// indices [16, 15, 14] to the complete Hilbert space.
///
/// Notes: assumes qubit indices are unique.
///
/// Done in preparation for arbitrary gate application on adjacent qubits.
fn permutation_arbitrary(qubit_inds: &[u64], n_qubits: u64) -> (Matrix, u64) {
    // Begin construction of permutation
    let mut perm = Array2::eye(2usize.pow(n_qubits as u32));
    // First, sort the list and find the median.
    let mut sorted_inds = qubit_inds.to_vec();
    sorted_inds.sort();
    let med_i = qubit_inds.len() / 2;
    let med = sorted_inds[med_i];
    // The starting position of all specified Hilbert spaces begins at the qubit at (median -
    // med_i)
    let start = med - med_i as u64;
    if qubit_inds.len() > 1 {
        // Array of final indices the arguments are mapped to, from high index to low index, left to
        // right ordering
        let final_map = (start..start + qubit_inds.len() as u64)
            .rev()
            .collect::<Vec<_>>();

        // Note that the lifting operation takes a k-qubit gate operating on the qubits i+k-1, i+k-2,
        // ... i (left to right). two_swap_helper can be used to build the permutation matrix by
        // filling out the final map by sweeping over the qubit_inds from left to right and back again,
        // swapping qubits into position. we loop over the qubit_inds until the final mapping matches
        // the argument.
        let mut qubit_arr = (0..n_qubits).collect::<Vec<_>>(); // Current qubit indexing

        let mut made_it = false;
        let mut right = true;
        while !made_it {
            let array = if right {
                (0..qubit_inds.len()).collect::<Vec<_>>()
            } else {
                (0..qubit_inds.len()).rev().collect()
            };

            for i in array {
                let j = qubit_arr
                    .iter()
                    .position(|&q| q == qubit_inds[i])
                    .expect("These arrays cover the same range.");
                let pmod = two_swap_helper(j as u64, final_map[i], n_qubits, &mut qubit_arr);
                perm = pmod.dot(&perm);
                if (final_map[final_map.len() - 1]..final_map[0] + 1)
                    .rev()
                    .zip(qubit_inds)
                    .all(|(f, &q)| qubit_arr[f as usize] == q)
                {
                    made_it = true;
                    break;
                }
            }
            right = !right;
        }
    }
    (perm, start)
}

/// Generate the permutation matrix that permutes two single-particle Hilbert spaces into adjacent
/// positions.
///
/// ALWAYS swaps j TO k. Recall that Hilbert spaces are ordered in decreasing qubit index order.
/// Hence, j > k implies that j is to the left of k.
///
/// End results:
///     j == k: nothing happens
///     j > k: Swap j right to k, until j at ind (k) and k at ind (k+1).
///     j < k: Swap j left to k, until j at ind (k) and k at ind (k-1).
///
/// Done in preparation for arbitrary 2-qubit gate application on ADJACENT qubits.
fn two_swap_helper(j: u64, k: u64, n_qubits: u64, qubit_map: &mut [u64]) -> Matrix {
    let mut perm = Array2::eye(2usize.pow(n_qubits as u32));
    let swap = CONSTANT_GATE_MATRICES
        .get("SWAP")
        .expect("Key should exist by design.");
    match Ord::cmp(&j, &k) {
        Ordering::Equal => {}
        Ordering::Greater => {
            // swap j right to k, until j at ind (k) and k at ind (k+1)
            for i in (k + 1..=j).rev() {
                perm = qubit_adjacent_lifted_gate(i - 1, swap, n_qubits).dot(&perm);
                qubit_map.swap(i as usize, (i - 1) as usize);
            }
        }
        Ordering::Less => {
            // swap j left to k, until j at ind (k) and k at ind (k-1)
            for i in j..k {
                perm = qubit_adjacent_lifted_gate(i, swap, n_qubits).dot(&perm);
                qubit_map.swap(i as usize, (i + 1) as usize);
            }
        }
    }
    perm
}

/// Lifts input k-qubit gate on adjacent qubits starting from qubit i to complete Hilbert space of
/// dimension 2 ** `num_qubits`.
///
/// Ex: 1-qubit gate, lifts from qubit i
/// Ex: 2-qubit gate, lifts from qubits (i+1, i)
/// Ex: 3-qubit gate, lifts from qubits (i+2, i+1, i), operating in that order
///
/// In general, this takes a k-qubit gate (2D matrix 2^k x 2^k) and lifts it to the complete
/// Hilbert space of dim 2^num_qubits, as defined by the right-to-left tensor product (1) in
/// arXiv:1608.03355.
///
/// Developer note: Quil and the QVM like qubits to be ordered such that qubit 0 is on the right.
/// Therefore, in `qubit_adjacent_lifted_gate`, `lifted_pauli`, and `lifted_state_operator`, we
/// build up the lifted matrix by performing the kronecker product from right to left.
///
/// Note that while the qubits are addressed in decreasing order, starting with num_qubit - 1 on
/// the left and ending with qubit 0 on the right (in a little-endian fashion), gates are still
/// lifted to apply on qubits in increasing index (right-to-left) order.
fn qubit_adjacent_lifted_gate(i: u64, matrix: &Matrix, n_qubits: u64) -> Matrix {
    let bottom_matrix = Array2::eye(2usize.pow(i as u32));
    let gate_size = (matrix.shape()[0] as f64).log2().floor() as u64;
    let top_qubits = n_qubits - i - gate_size;
    let top_matrix = Array2::eye(2usize.pow(top_qubits as u32));
    kron(&top_matrix, &kron(matrix, &bottom_matrix))
}

/// Gates matrices that don't use any parameters.
///
/// https://github.com/quil-lang/quil/blob/master/spec/Quil.md#standard-gates
static CONSTANT_GATE_MATRICES: Lazy<HashMap<String, Matrix>> = Lazy::new(|| {
    let _0 = real!(0.0);
    let _1 = real!(1.0);
    let _i = imag!(1.0);
    let _1_sqrt_2 = real!(std::f64::consts::FRAC_1_SQRT_2);
    HashMap::from([
        ("I".to_string(), Array2::eye(2)),
        ("X".to_string(), array![[_0, _1], [_1, _0]]),
        ("Y".to_string(), array![[_0, -_i], [_i, _0]]),
        ("Z".to_string(), array![[_1, _0], [_0, -_1]]),
        ("H".to_string(), array![[_1, _1], [_1, -_1]] * _1_sqrt_2),
        (
            "CNOT".to_string(),
            array![
                [_1, _0, _0, _0],
                [_0, _1, _0, _0],
                [_0, _0, _0, _1],
                [_0, _0, _1, _0]
            ],
        ),
        (
            "CCNOT".to_string(),
            array![
                [_1, _0, _0, _0, _0, _0, _0, _0],
                [_0, _1, _0, _0, _0, _0, _0, _0],
                [_0, _0, _1, _0, _0, _0, _0, _0],
                [_0, _0, _0, _1, _0, _0, _0, _0],
                [_0, _0, _0, _0, _1, _0, _0, _0],
                [_0, _0, _0, _0, _0, _1, _0, _0],
                [_0, _0, _0, _0, _0, _0, _0, _1],
                [_0, _0, _0, _0, _0, _0, _1, _0],
            ],
        ),
        ("S".to_string(), array![[_1, _0], [_0, _i]]),
        (
            "T".to_string(),
            array![[_1, _0], [_0, Complex64::cis(std::f64::consts::FRAC_PI_4)]],
        ),
        ("CZ".to_string(), {
            let mut cz = Array2::eye(4);
            cz[[3, 3]] = -_1;
            cz
        }),
        (
            "SWAP".to_string(),
            array![
                [_1, _0, _0, _0],
                [_0, _0, _1, _0],
                [_0, _1, _0, _0],
                [_0, _0, _0, _1],
            ],
        ),
        (
            "CSWAP".to_string(),
            array![
                [_1, _0, _0, _0, _0, _0, _0, _0],
                [_0, _1, _0, _0, _0, _0, _0, _0],
                [_0, _0, _1, _0, _0, _0, _0, _0],
                [_0, _0, _0, _1, _0, _0, _0, _0],
                [_0, _0, _0, _0, _1, _0, _0, _0],
                [_0, _0, _0, _0, _0, _0, _1, _0],
                [_0, _0, _0, _0, _0, _1, _0, _0],
                [_0, _0, _0, _0, _0, _0, _0, _1],
            ],
        ),
        (
            "ISWAP".to_string(),
            array![
                [_1, _0, _0, _0],
                [_0, _0, _i, _0],
                [_0, _i, _0, _0],
                [_0, _0, _0, _1],
            ],
        ),
    ])
});

type ParameterizedMatrix = fn(Complex64) -> Matrix;

/// Gates matrices that use parameters.
///
/// https://github.com/quil-lang/quil/blob/master/spec/Quil.md#standard-gates
static PARAMETERIZED_GATE_MATRICES: Lazy<HashMap<String, ParameterizedMatrix>> = Lazy::new(|| {
    // Unfortunately, Complex::cis takes a _float_ argument.
    HashMap::from([
        (
            "RX".to_string(),
            (|theta: Complex64| {
                let _i = imag!(1.0);
                let t = theta / 2.0;
                array![[t.cos(), -_i * t.sin()], [-_i * t.sin(), t.cos()]]
            }) as ParameterizedMatrix,
        ),
        (
            "RY".to_string(),
            (|theta: Complex64| {
                let t = theta / 2.0;
                array![[t.cos(), -t.sin()], [t.sin(), t.cos()]]
            }) as ParameterizedMatrix,
        ),
        (
            "RZ".to_string(),
            (|theta: Complex64| {
                let t = theta / 2.0;
                array![[t.cos(), -t.sin()], [t.sin(), t.cos()]]
            }) as ParameterizedMatrix,
        ),
        (
            "PHASE".to_string(),
            (|alpha: Complex64| {
                let mut p = Array2::eye(2);
                p[[1, 1]] = alpha.cos() + imag!(1.0) * alpha.sin();
                p
            }) as ParameterizedMatrix,
        ),
        (
            "CPHASE00".to_string(),
            (|alpha: Complex64| {
                let mut p = Array2::eye(4);
                p[[0, 0]] = alpha.cos() + imag!(1.0) * alpha.sin();
                p
            }) as ParameterizedMatrix,
        ),
        (
            "CPHASE01".to_string(),
            (|alpha: Complex64| {
                let mut p = Array2::eye(4);
                p[[1, 1]] = alpha.cos() + imag!(1.0) * alpha.sin();
                p
            }) as ParameterizedMatrix,
        ),
        (
            "CPHASE10".to_string(),
            (|alpha: Complex64| {
                let mut p = Array2::eye(4);
                p[[2, 2]] = alpha.cos() + imag!(1.0) * alpha.sin();
                p
            }) as ParameterizedMatrix,
        ),
        (
            "CPHASE".to_string(),
            (|alpha: Complex64| {
                let mut p = Array2::eye(4);
                p[[3, 3]] = alpha.cos() + imag!(1.0) * alpha.sin();
                p
            }) as ParameterizedMatrix,
        ),
        (
            "PSWAP".to_string(),
            (|theta: Complex64| {
                let (_0, _1, _c) = (real!(0.0), real!(1.0), theta.cos() + theta);
                array![
                    [_1, _0, _0, _0],
                    [_0, _0, _c, _0],
                    [_0, _c, _0, _0],
                    [_0, _0, _0, _1],
                ]
            }) as ParameterizedMatrix,
        ),
    ])
});

impl Quil for Gate {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        for modifier in &self.modifiers {
            modifier.write(f, fall_back_to_debug)?;
            write!(f, " ")?;
        }

        write!(f, "{}", self.name)?;
        write_expression_parameter_string(f, fall_back_to_debug, &self.parameters)?;
        write_qubits(f, fall_back_to_debug, &self.qubits)
    }
}

impl Quil for GateModifier {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match self {
            Self::Controlled => write!(f, "CONTROLLED"),
            Self::Dagger => write!(f, "DAGGER"),
            Self::Forked => write!(f, "FORKED"),
        }
        .map_err(Into::into)
    }
}

#[cfg(test)]
mod test_gate_into_matrix {
    use super::{
        lifted_gate_matrix, permutation_arbitrary, qubit_adjacent_lifted_gate, two_swap_helper,
        Expression::Number, Gate, GateModifier::*, Matrix, ParameterizedMatrix, Qubit::Fixed,
        CONSTANT_GATE_MATRICES, PARAMETERIZED_GATE_MATRICES,
    };
    use crate::{imag, real};
    use approx::assert_abs_diff_eq;
    use ndarray::{array, linalg::kron, Array2};
    use num_complex::Complex64;
    use once_cell::sync::Lazy;
    use rstest::rstest;

    static _0: Complex64 = real!(0.0);
    static _1: Complex64 = real!(1.0);
    static _I: Complex64 = imag!(1.0);
    static PI: Complex64 = real!(std::f64::consts::PI);
    static PI_4: Complex64 = real!(std::f64::consts::FRAC_PI_4);
    static SWAP: Lazy<Matrix> = Lazy::new(|| CONSTANT_GATE_MATRICES.get("SWAP").cloned().unwrap());
    static X: Lazy<Matrix> = Lazy::new(|| array![[_0, _1], [_1, _0]]);
    static P0: Lazy<Matrix> = Lazy::new(|| array![[_1, _0], [_0, _0]]);
    static P1: Lazy<Matrix> = Lazy::new(|| array![[_0, _0], [_0, _1]]);
    static CNOT: Lazy<Matrix> = Lazy::new(|| CONSTANT_GATE_MATRICES.get("CNOT").cloned().unwrap());
    static ISWAP: Lazy<Matrix> =
        Lazy::new(|| CONSTANT_GATE_MATRICES.get("ISWAP").cloned().unwrap());
    static H: Lazy<Matrix> = Lazy::new(|| CONSTANT_GATE_MATRICES.get("H").cloned().unwrap());
    static RZ: Lazy<ParameterizedMatrix> =
        Lazy::new(|| PARAMETERIZED_GATE_MATRICES.get("RZ").cloned().unwrap());
    static CCNOT: Lazy<Matrix> =
        Lazy::new(|| CONSTANT_GATE_MATRICES.get("CCNOT").cloned().unwrap());
    static CZ: Lazy<Matrix> = Lazy::new(|| CONSTANT_GATE_MATRICES.get("CZ").cloned().unwrap());

    #[rstest]
    #[case(0, 2, &SWAP)]
    #[case(0, 3, &kron(&Array2::eye(2), &SWAP))]
    #[case(0, 4, &kron(&Array2::eye(4), &SWAP))]
    #[case(1, 3, &kron(&SWAP, &Array2::eye(2)))]
    #[case(1, 4, &kron(&Array2::eye(2), &kron(&SWAP, &Array2::eye(2))))]
    #[case(2, 4, &kron(&Array2::eye(1), &kron(&SWAP, &Array2::eye(4))))]
    #[case(8, 10, &kron(&Array2::eye(1), &kron(&SWAP, &Array2::eye(2usize.pow(8)))))]
    fn test_qubit_adjacent_lifted_gate(
        #[case] i: u64,
        #[case] n_qubits: u64,
        #[case] expected: &Matrix,
    ) {
        let result = qubit_adjacent_lifted_gate(i, &SWAP, n_qubits);
        assert_abs_diff_eq!(result, expected);
    }

    // test cases via pyquil.simulation.tools.two_swap_helper
    #[rstest]
    #[case(0, 0, 2, &mut[0, 1], &[0, 1], &Array2::eye(4))]
    #[case(0, 1, 2, &mut[0, 1], &[1, 0], &array![[_1, _0, _0, _0],
                                                 [_0, _0, _1, _0],
                                                 [_0, _1, _0, _0],
                                                 [_0, _0, _0, _1]])]
    #[case(0, 1, 2, &mut[1, 0], &[0, 1], &array![[_1, _0, _0, _0],
                                                 [_0, _0, _1, _0],
                                                 [_0, _1, _0, _0],
                                                 [_0, _0, _0, _1]])]
    #[case(1, 0, 2, &mut[0, 1], &[1, 0], &array![[_1, _0, _0, _0],
                                                 [_0, _0, _1, _0],
                                                 [_0, _1, _0, _0],
                                                 [_0, _0, _0, _1]])]
    #[case(1, 0, 2, &mut[1, 0], &[0, 1], &array![[_1, _0, _0, _0],
                                                 [_0, _0, _1, _0],
                                                 [_0, _1, _0, _0],
                                                 [_0, _0, _0, _1]])]
    #[case(0, 1, 3, &mut[0, 1, 2], &[1, 0, 2], &array![[_1, _0, _0, _0, _0, _0, _0, _0],
                                                       [_0, _0, _1, _0, _0, _0, _0, _0],
                                                       [_0, _1, _0, _0, _0, _0, _0, _0],
                                                       [_0, _0, _0, _1, _0, _0, _0, _0],
                                                       [_0, _0, _0, _0, _1, _0, _0, _0],
                                                       [_0, _0, _0, _0, _0, _0, _1, _0],
                                                       [_0, _0, _0, _0, _0, _1, _0, _0],
                                                       [_0, _0, _0, _0, _0, _0, _0, _1]])]

    fn test_two_swap_helper(
        #[case] j: u64,
        #[case] k: u64,
        #[case] n_qubits: u64,
        #[case] qubit_map: &mut [u64],
        #[case] expected_qubit_map: &[u64],
        #[case] expected_matrix: &Matrix,
    ) {
        let result = two_swap_helper(j, k, n_qubits, qubit_map);
        assert_eq!(qubit_map, expected_qubit_map);
        assert_abs_diff_eq!(result, expected_matrix);
    }

    // test cases via pyquil.simulation.tools.permutation_arbitrary
    #[rstest]
    #[case(&[0], 1, 0, &Array2::eye(2))]
    #[case(&[0, 1], 2, 0, &array![[_1, _0, _0, _0],
                                  [_0, _0, _1, _0],
                                  [_0, _1, _0, _0],
                                  [_0, _0, _0, _1]])]
    #[case(&[1, 0], 2, 0, &Array2::eye(4))]
    #[case(&[0, 2], 3, 1, &array![[_1, _0, _0, _0, _0, _0, _0, _0],
                                  [_0, _0, _1, _0, _0, _0, _0, _0],
                                  [_0, _0, _0, _0, _1, _0, _0, _0],
                                  [_0, _0, _0, _0, _0, _0, _1, _0],
                                  [_0, _1, _0, _0, _0, _0, _0, _0],
                                  [_0, _0, _0, _1, _0, _0, _0, _0],
                                  [_0, _0, _0, _0, _0, _1, _0, _0],
                                  [_0, _0, _0, _0, _0, _0, _0, _1]])]
    #[case(&[1, 2], 3, 1, &array![[_1, _0, _0, _0, _0, _0, _0, _0],
                                  [_0, _1, _0, _0, _0, _0, _0, _0],
                                  [_0, _0, _0, _0, _1, _0, _0, _0],
                                  [_0, _0, _0, _0, _0, _1, _0, _0],
                                  [_0, _0, _1, _0, _0, _0, _0, _0],
                                  [_0, _0, _0, _1, _0, _0, _0, _0],
                                  [_0, _0, _0, _0, _0, _0, _1, _0],
                                  [_0, _0, _0, _0, _0, _0, _0, _1]])]
    #[case(&[0, 1, 2], 3, 0, &array![[_1, _0, _0, _0, _0, _0, _0, _0],
                                     [_0, _0, _0, _0, _1, _0, _0, _0],
                                     [_0, _0, _1, _0, _0, _0, _0, _0],
                                     [_0, _0, _0, _0, _0, _0, _1, _0],
                                     [_0, _1, _0, _0, _0, _0, _0, _0],
                                     [_0, _0, _0, _0, _0, _1, _0, _0],
                                     [_0, _0, _0, _1, _0, _0, _0, _0],
                                     [_0, _0, _0, _0, _0, _0, _0, _1]])]
    fn test_permutation_arbitrary(
        #[case] qubit_inds: &[u64],
        #[case] n_qubits: u64,
        #[case] expected_start: u64,
        #[case] expected_matrix: &Matrix,
    ) {
        let (result_matrix, result_start) = permutation_arbitrary(qubit_inds, n_qubits);
        assert_abs_diff_eq!(result_matrix, expected_matrix);
        assert_eq!(result_start, expected_start);
    }

    #[rstest]
    #[case(&CNOT, &mut [1, 0], 2, &(kron(&P0, &Array2::eye(2)) + kron(&P1, &X)))]
    #[case(&CNOT, &mut [0, 1], 2, &(kron(&Array2::eye(2), &P0) + kron(&X, &P1)))]
    #[case(&CNOT, &mut [2, 1], 3, &(kron(&CNOT, &Array2::eye(2))))]
    #[case(&ISWAP, &mut [0, 1], 3, &kron(&Array2::eye(2), &ISWAP))]
    #[case(&ISWAP, &mut [1, 0], 3, &kron(&Array2::eye(2), &ISWAP))]
    #[case(&ISWAP, &mut [1, 2], 4, &kron(&Array2::eye(2), &kron(&ISWAP, &Array2::eye(2))))]
    #[case(&ISWAP, &mut [3, 2], 4, &kron(&ISWAP, &Array2::eye(4)))]
    #[case(&ISWAP, &mut [2, 3], 4, &kron(&ISWAP, &Array2::eye(4)))]
    #[case(&H, &mut [0], 4, &kron(&Array2::eye(8), &H))]
    #[case(&H, &mut [1], 4, &kron(&Array2::eye(4), &kron(&H, &Array2::eye(2))))]
    #[case(&H, &mut [2], 4, &kron(&Array2::eye(2), &kron(&H, &Array2::eye(4))))]
    #[case(&H, &mut [3], 4, &kron(&H, &Array2::eye(8)))]
    #[case(&H, &mut [0], 5, &kron(&Array2::eye(16), &H))]
    #[case(&H, &mut [1], 5, &kron(&Array2::eye(8), &kron(&H, &Array2::eye(2))))]
    #[case(&H, &mut [2], 5, &kron(&Array2::eye(4), &kron(&H, &Array2::eye(4))))]
    #[case(&H, &mut [3], 5, &kron(&Array2::eye(2), &kron(&H, &Array2::eye(8))))]
    #[case(&H, &mut [4], 5, &kron(&H, &Array2::eye(16)))]
    fn test_lifted_gate_matrix(
        #[case] matrix: &Matrix,
        #[case] indices: &mut [u64],
        #[case] n_qubits: u64,
        #[case] expected: &Matrix,
    ) {
        assert_abs_diff_eq!(lifted_gate_matrix(matrix, indices, n_qubits), expected);
    }

    #[rstest]
    #[case(&mut Gate::new("H", vec![], vec![Fixed(0)], vec![]).unwrap(), 4, &kron(&Array2::eye(8), &H))]
    #[case(&mut Gate::new("RZ", vec![Number(PI_4)], vec![Fixed(0)], vec![Dagger]).unwrap(), 1, &RZ(-PI_4))]
    #[case(&mut Gate::new("X", vec![], vec![Fixed(0)], vec![Dagger]).unwrap().controlled(Fixed(1)), 2, &CNOT)]
    #[case(
        &mut Gate::new("X", vec![], vec![Fixed(0)], vec![]).unwrap().dagger().controlled(Fixed(1)).dagger().dagger().controlled(Fixed(2)),
        3,
        &CCNOT
    )]
    #[case(
        &mut Gate::new("PHASE", vec![Number(_0)], vec![Fixed(1)], vec![]).unwrap().forked(Fixed(0), vec![Number(PI)]).unwrap(),
        2,
        &lifted_gate_matrix(&CZ, &[0, 1], 2)
    )]
    fn test_to_unitary(#[case] gate: &mut Gate, #[case] n_qubits: u64, #[case] expected: &Matrix) {
        let result = gate.to_unitary(n_qubits);
        assert!(result.is_ok());
        assert_abs_diff_eq!(result.as_ref().unwrap(), expected);
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, strum::Display, strum::EnumString)]
#[strum(serialize_all = "UPPERCASE")]
pub enum PauliGate {
    I,
    X,
    Y,
    Z,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PauliTerm {
    pub arguments: Vec<(PauliGate, String)>,
    pub expression: Expression,
}

impl PauliTerm {
    pub fn new(arguments: Vec<(PauliGate, String)>, expression: Expression) -> Self {
        Self {
            arguments,
            expression,
        }
    }

    pub(crate) fn word(&self) -> impl Iterator<Item = &PauliGate> {
        self.arguments.iter().map(|(gate, _)| gate)
    }

    pub(crate) fn arguments(&self) -> impl Iterator<Item = &String> {
        self.arguments.iter().map(|(_, argument)| argument)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PauliSum {
    pub arguments: Vec<String>,
    pub terms: Vec<PauliTerm>,
}

impl PauliSum {
    pub fn new(arguments: Vec<String>, terms: Vec<PauliTerm>) -> Result<Self, GateError> {
        let diff = terms
            .iter()
            .flat_map(|t| t.arguments())
            .collect::<HashSet<_>>()
            .difference(&arguments.iter().collect::<HashSet<_>>())
            .copied()
            .collect::<Vec<_>>();

        if !diff.is_empty() {
            return Err(GateError::PauliSumArgumentMismatch {
                mismatches: diff.into_iter().cloned().collect(),
                expected_arguments: arguments,
            });
        }

        Ok(Self { arguments, terms })
    }
}

/// An enum representing a the specification of a [`GateDefinition`] for a given [`GateType`]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GateSpecification {
    /// A matrix of [`Expression`]s representing a unitary operation for a [`GateType::Matrix`].
    Matrix(Vec<Vec<Expression>>),
    /// A vector of integers that defines the permutation used for a [`GateType::Permutation`]
    Permutation(Vec<u64>),
    /// A Hermitian operator specified as a Pauli sum, a sum of combinations of Pauli operators,
    /// used for a [`GateType::PauliSum`]
    PauliSum(PauliSum),
}

impl Quil for GateSpecification {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match self {
            GateSpecification::Matrix(matrix) => {
                for row in matrix {
                    write!(f, "\t")?;
                    write_join_quil(f, fall_back_to_debug, row.iter(), ", ", "")?;
                    writeln!(f)?;
                }
            }
            GateSpecification::Permutation(permutation) => {
                write!(f, "\t")?;
                if let Some(i) = permutation.first() {
                    write!(f, "{i}")?;
                }
                for i in permutation.iter().skip(1) {
                    write!(f, ", {i}")?;
                }
                writeln!(f)?;
            }
            GateSpecification::PauliSum(pauli_sum) => {
                for term in &pauli_sum.terms {
                    write!(f, "\t")?;
                    for word in term.word() {
                        write!(f, "{word}")?;
                    }
                    write!(f, "(")?;
                    term.expression.write(f, fall_back_to_debug)?;
                    write!(f, ")")?;
                    for argument in term.arguments() {
                        write!(f, " {argument}")?;
                    }
                    writeln!(f)?;
                }
            }
        }
        Ok(())
    }
}

/// A struct encapsulating a quil Gate Definition
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GateDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub specification: GateSpecification,
}

impl GateDefinition {
    pub fn new(
        name: String,
        parameters: Vec<String>,
        specification: GateSpecification,
    ) -> Result<Self, GateError> {
        validate_user_identifier(&name)?;
        Ok(Self {
            name,
            parameters,
            specification,
        })
    }
}

impl Quil for GateDefinition {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "DEFGATE {}", self.name,)?;
        write_parameter_string(f, &self.parameters)?;
        match &self.specification {
            GateSpecification::Matrix(_) => writeln!(f, " AS MATRIX:")?,
            GateSpecification::Permutation(_) => writeln!(f, " AS PERMUTATION:")?,
            GateSpecification::PauliSum(sum) => {
                for arg in &sum.arguments {
                    write!(f, " {arg}")?;
                }
                writeln!(f, " AS PAULI-SUM:")?
            }
        }
        self.specification.write(f, fall_back_to_debug)?;
        Ok(())
    }
}

#[cfg(test)]
mod test_gate_definition {
    use super::{GateDefinition, GateSpecification, PauliGate, PauliSum, PauliTerm};
    use crate::expression::{
        Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
        PrefixExpression, PrefixOperator,
    };
    use crate::quil::Quil;
    use crate::{imag, real};
    use insta::assert_snapshot;
    use rstest::rstest;

    #[rstest]
    #[case(
        "Permutation GateDefinition",
        GateDefinition{
            name: "PermGate".to_string(),
            parameters: vec![],
            specification: GateSpecification::Permutation(vec![0, 1, 2, 3, 4, 5, 7, 6]),

        }
    )]
    #[case(
        "Parameterized GateDefinition",
        GateDefinition{
            name: "ParamGate".to_string(),
            parameters: vec!["theta".to_string()],
            specification: GateSpecification::Matrix(vec![
                vec![
                    Expression::FunctionCall(FunctionCallExpression {
                        function: crate::expression::ExpressionFunction::Cosine,
                        expression: Box::new(Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(imag!(1f64)))
                        })),
                        operator: InfixOperator::Star,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: ExpressionFunction::Sine,
                            expression: Box::new(Expression::Infix(InfixExpression {
                                left: Box::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: Box::new(Expression::Number(real!(2.0))),
                            })),
                        })),
                    })
                ],
                vec![
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(imag!(1f64)))
                        })),
                        operator: InfixOperator::Star,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: ExpressionFunction::Sine,
                            expression: Box::new(Expression::Infix(InfixExpression {
                                left: Box::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: Box::new(Expression::Number(real!(2.0))),
                            })),
                        })),
                    }),
                    Expression::FunctionCall(FunctionCallExpression {
                        function: crate::expression::ExpressionFunction::Cosine,
                        expression: Box::new(Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                ],
            ]),

        }
    )]
    #[case(
        "Pauli Sum GateDefinition",
        GateDefinition{
            name: "PauliSumGate".to_string(),
            parameters: vec!["theta".to_string()],
            specification: GateSpecification::PauliSum(PauliSum{arguments: vec!["p".to_string(), "q".to_string()], terms: vec![
                PauliTerm {
                    arguments: vec![(PauliGate::Z, "p".to_string()), (PauliGate::Z, "q".to_string())],
                    expression: Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Variable("theta".to_string()))
                        })),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::Number(real!(4.0)))
                    }),
                },
                PauliTerm {
                    arguments: vec![(PauliGate::Y, "p".to_string())],
                    expression: Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Variable("theta".to_string())),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::Number(real!(4.0)))
                    }),
                },
                PauliTerm {
                    arguments: vec![(PauliGate::X, "q".to_string())],
                    expression: Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Variable("theta".to_string())),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::Number(real!(4.0)))
                    }),
                },
            ]})
        }
    )]
    fn test_display(#[case] description: &str, #[case] gate_def: GateDefinition) {
        insta::with_settings!({
            snapshot_suffix => description,
        }, {
            assert_snapshot!(gate_def.to_quil_or_debug())
        })
    }
}

/// The type of a [`GateDefinition`]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GateType {
    Matrix,
    Permutation,
    PauliSum,
}

impl Quil for GateType {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match self {
            Self::Matrix => write!(f, "MATRIX"),
            Self::Permutation => write!(f, "PERMUTATION"),
            Self::PauliSum => write!(f, "PAULI-SUM"),
        }
        .map_err(Into::into)
    }
}
