//! Types for tracking read and write dependencies in a control flow graph.
//!
//! During scheduling, we want to be able to order a variety of operations whose semantics are
//! *sequentially consistent*: all "reads" must see the *same* most recent "write", for the
//! appropriate notion of "read" and "write".  The [`DependencyQueue`] type allows enforcing this
//! constraint, with the notion of "read" and "write" being given by the [`Access`] trait.

use std::{collections::HashSet, fmt::Debug, hash::Hash};

use super::{
    InstructionFrameInteraction, MemoryAccessDependency, MemoryAccessType, ScheduledGraphNode,
};

/// A type representing the kind of access being made to some resource.
///
/// These accesses are made by [actions][Self::Action], which can be [divided into][Self::classify]
/// [*reads*][Self::Read] and [*writes*][Self::Write].  Read and write accesses need different
/// constraints to enforce sequential consistency; for details, see the [`DependencyQueue`] type.
///
/// For instance, if we're considering writes to memory, then:
///
/// - [Actions][Self::Action] are program instructions.
///
/// - [Read][Self::Read] and [write][Self::Write] actions are also plain instructions, since they
///   don't need to track any extra data.
///
/// - The resulting [dependencies][Self::Dependency] are pairs consisting of a program instruction
///   and an enum value telling you whether that instruction performed a read or a write.
///
/// The trait bounds on this type and its associated types are largely for convenient `derive`-ing
/// of traits given the particular implementations we have.
pub(super) trait Access: Copy + Eq + Hash + Debug {
    /// An action is a value that can perform an access to the resource being monitored.
    type Action: Copy + Eq + Hash + Debug;

    /// How read actions are stored.
    type Read: Copy + Eq + Hash + Debug;

    /// How write actions are stored.
    type Write: Copy + Eq + Hash + Debug;

    /// How dependencies on actions are reported.
    type Dependency: Copy + Eq + Hash + Debug;

    /// Returns the implicit initial write action, if there is one.
    fn initial_writer() -> Option<Self::Write>;

    /// Given an access type and an action, indicate whether that action was a read or a write of
    /// the resource being monitored.
    fn classify(self, action: Self::Action) -> AccessingAction<Self>;

    /// Convert a read action into a reported dependency.
    fn read_dependency(read: Self::Read) -> Self::Dependency;

    /// Convert a write action into a reported dependency.
    fn write_dependency(write: Self::Write) -> Self::Dependency;
}

/// Records whether an [action][Access::Action] is a read action or a write action.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum AccessingAction<A: Access> {
    /// A read action
    Read(A::Read),

    /// A writeaction
    Write(A::Write),
}

/// A [`DependencyQueue`] for [`MemoryAccessType`] keeps track of which reads and writes are
/// currently outstanding for a given memory region.
///
/// When ordering memory accesses, [reads][MemoryAccessType::Read] are considered
/// [reads][Self::Read], and both [writes][MemoryAccessType::Write] and
/// [captures][MemoryAccessType::Capture] are considered [writes][Self::Write].
impl Access for MemoryAccessType {
    /// Instructions (by identifier) that have accessed a memory region.
    type Action = ScheduledGraphNode;

    /// Read dependencies; there is only one kind of read dependency, so these are also just
    /// instructions.
    type Read = ScheduledGraphNode;

    /// Write dependencies; these are stored as a full [`MemoryAccessDependency`] so they can
    /// remember whether the instruction performed as [classical write][MemoryAccessType::Write] or
    /// a [capture][MemoryAccessType::Capture].
    type Write = MemoryAccessDependency;

    /// Dependencies, which record the instruction that performed the memory access
    /// ([`Self::Action`]) as well as what type of access it was ([`Self`]).
    type Dependency = MemoryAccessDependency;

    /// Memory regions do not start with an implicit pending writer; the state of memory is stable.
    #[inline]
    fn initial_writer() -> Option<<Self as Access>::Write> {
        None
    }

    #[inline]
    fn classify(self, action: Self::Action) -> AccessingAction<Self> {
        match self {
            Self::Read => AccessingAction::Read(action),
            Self::Write | Self::Capture => AccessingAction::Write(MemoryAccessDependency {
                access_type: self,
                node_id: action,
            }),
        }
    }

    #[inline]
    fn read_dependency(read: <Self as Access>::Read) -> Self::Dependency {
        MemoryAccessDependency {
            access_type: Self::Read,
            node_id: read,
        }
    }

    #[inline]
    fn write_dependency(write: <Self as Access>::Write) -> Self::Dependency {
        write
    }
}

/// A [`DependencyQueue`] for [`InstructionFrameInteraction`] keeps track of which instructions are
/// currently blocking or using a given frame.
///
/// In this case, "depends on" means "must execute at or after completion of".  The specific
/// interpretation of "at or after" depends on the type of dependency and the compiler.
impl Access for InstructionFrameInteraction {
    /// Instructions (by identifier) that are interacting with the managed frame.
    type Action = ScheduledGraphNode;

    /// Instructions that block this frame.  Blocking a frame is analogous to reading from
    /// it, as multiple concurrent blocks do not conflict but any block conflicts with a use.
    type Read = ScheduledGraphNode;

    /// Instructions that use this frame.  Using a frame is analogous to writing to it, as only one
    /// use of a frame may occur at a time.
    type Write = ScheduledGraphNode;

    /// Dependencies, which simply report the instructions that are interacting with a frame without
    /// keeping track of whether this is a blocking or using dependency.
    type Dependency = ScheduledGraphNode;

    /// When tracking frame accesses, it is useful to consider the start of the basic block as
    /// implicitly using all frames.
    ///
    /// This gives us two guarantees:
    ///
    /// 1. No instruction can be scheduled prior to the start of the instruction block.
    ///
    /// 2. All scheduled instructions within the block depend on the block's start time, at least
    ///    indirectly.
    #[inline]
    fn initial_writer() -> Option<Self::Write> {
        Some(ScheduledGraphNode::BlockStart)
    }

    #[inline]
    fn classify(self, action: Self::Action) -> AccessingAction<Self> {
        match self {
            Self::Blocking => AccessingAction::Read(action),
            Self::Using => AccessingAction::Write(action),
        }
    }

    #[inline]
    fn read_dependency(read: Self::Read) -> Self::Dependency {
        read
    }

    #[inline]
    fn write_dependency(write: Self::Write) -> Self::Dependency {
        write
    }
}
/// A [`DependencyQueue`] keeps track of which reads and writes are currently outstanding for a
/// given resource.
///
/// Obeying the dependencies reported by a dependency queue ensures that access to that resource is
/// *sequentially consistent*.  It does this by guaranteeing the following things:
///
/// 1. All writes must happen-before subsequent reads.
/// 2. All reads must happen-before subsequent writes.
/// 3. There are no dependencies between reads.
///
/// This allows scheduling to reorder reads without allowing them to lose track of where their
/// value came from.
///
/// This queue keeps track of the most recent write access to the resource, if any, as well as all
/// reads that have been performed since that write.  Every access (read or write)
/// [recorded][Self::record_access_and_get_dependencies] after that write access incurs a dependency
/// on said write.  When a subsequent write access is performed, it incurs as a dependency both the
/// prior most recent write as well as all reads between the two; this new write becomes the most recent
/// write, with no reads that have been performed since it.
///
/// # Examples
///
/// If there are two writers, the second has a dependency on the first:
///
/// ```text
/// writer 1 ───▶ writer 2
/// ```
///
/// If there are multiple readers, they all have a dependency on the most recent writer, but no
/// relationship to each other:
///
/// ```text
/// writer 1 ─┬──▶ reader 1
///           ├──▶ reader 2
///           └──▶ reader 3
/// ```
///
/// If there are both readers and writers that are interleaved, then we can get a writer that both
/// has a reader as a dependency and is itself a dependency of a reader, producing an ordering
/// constraint between two readers even though readers never get direct dependency edges between
/// each other:
///
/// ```text
/// reader 1 ───▶ writer 1 ───▶ reader 2
/// ```
pub(super) struct DependencyQueue<A: Access> {
    /// The most recent write access, if any.
    write: Option<A::Write>,

    /// All reads that that have occurred after [`Self::write`] and before any other write access.
    reads: HashSet<A::Read>,
}

impl<A: Access> DependencyQueue<A> {
    /// Create a new [`DependencyQueue`] containing no readers and only the [implicit initial
    /// writer][Access::initial_writer], if any.
    pub(super) fn new() -> Self {
        Self {
            write: A::initial_writer(),
            reads: HashSet::new(),
        }
    }

    /// Record that an action is performing an access of the provided type to the managed resource,
    /// returning all the accesses that must have completed before this one can happen.
    ///
    /// To ensure sequential consistency, writes may not happen concurrently with any other accesses
    /// to the managed resource; however, multiple reads may be concurrent with each other if this
    /// violates no other constraints.
    pub(super) fn record_access_and_get_dependencies(
        &mut self,
        action: A::Action,
        access_type: A,
    ) -> HashSet<A::Dependency> {
        // Writes must happen before both subsequent reads *and* subsequent writes, so we
        // unconditionally register a dependency on the preceding write.
        let mut result: HashSet<_> = self.write.into_iter().map(A::write_dependency).collect();

        match access_type.classify(action) {
            // If we're doing a write now, then all outstanding reads must finish first.
            AccessingAction::Write(write) => {
                result.extend(self.reads.drain().map(A::read_dependency));
                self.write = Some(write);
            }
            // Otherwise, save this read so we can serialize it with respect to the next write.
            AccessingAction::Read(read) => {
                self.reads.insert(read);
            }
        }

        result
    }

    /// Consume the [`DependencyQueue`] and report all pending actions.
    ///
    /// These actions are ones that could potentially be dependencies of any future action performed
    /// on the managed resource.
    pub(super) fn into_pending_dependencies(self) -> HashSet<A::Dependency> {
        // We `collect` into a `HashSet` rather than returning an `impl Iterator` so that we get the
        // unique-items enforcement.
        self.reads
            .into_iter()
            .map(A::read_dependency)
            .chain(self.write.into_iter().map(A::write_dependency))
            .collect()
    }
}

/// Corresponds to [`DependencyQueue::new`].
impl<A: Access> Default for DependencyQueue<A> {
    fn default() -> Self {
        Self::new()
    }
}
