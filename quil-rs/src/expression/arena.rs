//! First attempt to implement Expressions via an arena.
use paste::paste;
use std::{
    collections::VecDeque,
    ops::{Add, AddAssign, BitXor, BitXorAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign},
};

// Invariants/good behavior:
// - only ever constructed or used in an arena
// - `.parent` is only ever None if this is the top node
// - `.first_child` only ever points to an index greater than where this Node is found in the arena
// - `.second_child` only ever points to an index greater than `.first_child`, and doesn't exist if
// `.first_child` doesn't.
#[derive(Clone, Debug)]
struct Node {
    kind: Kind,
    parent: Option<usize>,
    first_child: Option<usize>,
    second_child: Option<usize>,
}

#[derive(Clone, Debug)]
enum Kind {
    // Function calls
    Cis,
    Cosine,
    Exponent,
    Sine,
    SquareRoot,
    Neg,
    // Infix
    Caret,
    Plus,
    Minus,
    Slash,
    Star,
    Address(MemoryReference),
    Number(num_complex::Complex64),
    Variable(String),
}

#[derive(Clone, Debug)]
pub struct MemoryReference {
    pub name: String,
    pub index: u64,
}

// Nonempty by construction
#[derive(Clone, Debug, Default)]
struct Arena {
    nodes: VecDeque<Node>,
}

macro_rules! arena_func_call {
    ($($kind:ident),+) => {
        $(
            paste! {
                fn [<$kind:lower>](&self) -> Self {
                    let mut new = self.clone();
                    new.[<$kind:lower _mut>]();
                    new
                }
                fn [<$kind:lower _mut>](&mut self) {
                    self.shift_by_mut(1);
                    self.nodes.push_front(Node{
                        kind: Kind::$kind,
                        parent: None,
                        first_child: Some(1),
                        second_child: None,

                    })
                }
            }
        )+
    }
}

macro_rules! arena_infix {
    ($($kind:ident),+) => {
        $(
            paste! {
                fn [<$kind:lower>](&self, other: Self) -> Self {
                    let mut new = self.clone();
                    new.[<$kind:lower _mut>](other);
                    new
                }
                fn [<$kind:lower _mut>](&mut self, other: Self) {
                    let n = self.len();
                    self.shift_by_mut(1);
                    self.nodes.push_front(Node{
                        kind: Kind::$kind,
                        parent: None,
                        first_child: Some(1),
                        second_child: Some(n + 1),
                    });
                    self.extend(other.shift_by(n));
                }
            }
        )+
    }
}

macro_rules! arena_singleton {
    ($(($kind:ident, $value_type:ty)),+) => {
        $(
            paste! {
                fn [<$kind:lower>](value: $value_type) -> Self {
                    Self::new(Node {
                        kind: Kind::$kind(value),
                        parent: None,
                        first_child: None,
                        second_child: None
                    })
                }
            }
        )+
    };
}

impl Arena {
    fn extend(&mut self, other: Self) {
        self.nodes.extend(other.nodes);
    }
    fn len(&self) -> usize {
        self.nodes.len()
    }
    fn new(node: Node) -> Self {
        let mut nodes = VecDeque::with_capacity(1);
        nodes.push_back(node);
        Self { nodes }
    }
    fn shift_by_mut(&mut self, step: usize) {
        self.nodes.iter_mut().for_each(|node| {
            // set parent of base node
            if node.parent.is_none() {
                node.parent = Some(0)
            }
            node.first_child.iter_mut().for_each(|i| *i += step);
            node.second_child.iter_mut().for_each(|i| *i += step);
        });
    }
    fn shift_by(&self, step: usize) -> Self {
        let mut new = self.clone();
        new.shift_by_mut(step);
        new
    }
    arena_func_call!(Cis, Cosine, Exponent, Sine, SquareRoot, Neg);
    arena_infix!(Caret, Plus, Minus, Slash, Star);
    arena_singleton!(
        (Address, MemoryReference),
        (Number, num_complex::Complex64),
        (Variable, String)
    );
}

#[derive(Clone, Debug, Default)]
pub struct Expression {
    arena: Arena,
}

macro_rules! expr_func_call {
    ($($kind:ident),+) => {
        $(
            paste! {
                pub fn [<$kind:lower>](&self) -> Self {
                    let mut new = self.clone();
                    new.[<$kind:lower _mut>]();
                    new
                }
                pub fn [<$kind:lower _mut>](&mut self) {
                    self.arena.[<$kind:lower _mut>]();
                }
            }
        )+
    }
}

macro_rules! expr_infix {
    ($($kind:ident),+) => {
        $(
            paste! {
                pub fn [<$kind:lower>](&self, other: Self) -> Self {
                    let mut new = self.clone();
                    new.[<$kind:lower _mut>](other);
                    new
                }
                pub fn [<$kind:lower _mut>](&mut self, other: Self) {
                    self.arena.[<$kind:lower _mut>](other.arena);
                }
            }
        )+
    }
}

macro_rules! expr_singleton {
    ($(($kind:ident, $value_type:ty)),+) => {
        $(
            paste! {
                pub fn [<$kind:lower>](value: $value_type) -> Self {
                    Self { arena: Arena::[<$kind:lower>](value) }
                }
            }
        )+
    };
}

impl Expression {
    expr_func_call!(Cis, Cosine, Exponent, Sine, SquareRoot, Neg);
    expr_infix!(Caret, Plus, Minus, Slash, Star);
    expr_singleton!(
        (Address, MemoryReference),
        (Number, num_complex::Complex64),
        (Variable, String)
    );
}

macro_rules! impl_via {
    ($(($trait:ident, $method:ident)),+) => {
        $(
            paste! {
                impl $trait for Expression {
                    type Output = Self;
                    fn [<$trait:lower>](self, other: Self) -> Self::Output {
                        self.$method(other)
                    }
                }
                impl [<$trait Assign>] for Expression {
                    fn [<$trait:lower _assign>](&mut self, other: Self) {
                        self.[<$method _mut>](other)
                    }
                }

            }
        )+
    };
}

impl_via!(
    (BitXor, caret),
    (Add, plus),
    (Sub, minus),
    (Div, slash),
    (Mul, star)
);
