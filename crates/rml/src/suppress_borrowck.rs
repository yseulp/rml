//! See [suppress_borrowck].

use rustc_hir::def_id::DefId;
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{BasicBlock, BasicBlockData, Body, SourceInfo, Terminator, TerminatorKind},
    ty::TyCtxt,
};

use crate::util::{is_logic, is_spec};

/// Since spec functions or logic functions are never executed, we omit
/// ownership for them. There is no need to have references, clones, etc. We may
/// treat anything as [Copy].
///
/// Hence, we must supress borrow checking for spec functions. Here, we follow
/// Creusot and "trick" the compiler by replacing the body with an empty loop.
///
/// Naturally, we only do so for spec functions.
pub fn suppress_borrowck<'tcx>(tcx: TyCtxt<'tcx>, did: DefId, body: &mut Body<'tcx>) {
    if should_suppress_borrowck(tcx, did) {
        *body.basic_blocks_mut() = make_loop(tcx);
        body.var_debug_info = Vec::new();
    }
}

/// Whether to suppress borrow checking. We do so for logic or spec functions.
fn should_suppress_borrowck<'tcx>(tcx: TyCtxt<'tcx>, did: DefId) -> bool {
    is_logic(tcx, did) || is_spec(tcx, did)
}

/// Create an empty loop.
pub(crate) fn make_loop<'tcx>(_: TyCtxt<'tcx>) -> IndexVec<BasicBlock, BasicBlockData<'tcx>> {
    let mut body = IndexVec::new();
    body.push(BasicBlockData::new(
        Some(Terminator {
            source_info: SourceInfo::outermost(rustc_span::DUMMY_SP),
            kind: TerminatorKind::Return,
        }),
        false,
    ));
    body
}
