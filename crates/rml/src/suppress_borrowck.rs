use rustc_hir::def_id::DefId;
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{BasicBlock, BasicBlockData, Body, SourceInfo, Terminator, TerminatorKind},
    ty::TyCtxt,
};

use crate::util::{is_logic, is_spec};

pub(crate) fn suppress_borrowck<'tcx>(tcx: TyCtxt<'tcx>, did: DefId, body: &mut Body<'tcx>) {
    if should_suppress_borrowck(tcx, did) {
        *body.basic_blocks_mut() = make_loop(tcx);
        body.var_debug_info = Vec::new();
    }
}

fn should_suppress_borrowck<'tcx>(tcx: TyCtxt<'tcx>, did: DefId) -> bool {
    is_logic(tcx, did) || is_spec(tcx, did)
}

pub(crate) fn make_loop<'tcx>(_: TyCtxt<'tcx>) -> IndexVec<BasicBlock, BasicBlockData<'tcx>> {
    let mut body = IndexVec::new();
    body.push(BasicBlockData::new(Some(Terminator {
        source_info: SourceInfo::outermost(rustc_span::DUMMY_SP),
        kind: TerminatorKind::Return,
    })));
    body
}
