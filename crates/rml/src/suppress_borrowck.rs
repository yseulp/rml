//! See [suppress_borrowck].
//! Some of the logic is taken directly from Creusot.

use std::collections::HashMap;

use indexmap::IndexSet;
use rustc_abi::FieldIdx;
use rustc_hir::def_id::DefId;
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{
        self, AggregateKind, BasicBlock, BasicBlockData, Body, Local, Location, Rvalue, SourceInfo,
        StatementKind, Terminator, TerminatorKind, visit::MutVisitor,
    },
    ty::TyCtxt,
};

use crate::util::{is_ghost, is_logic, is_snapshot, is_spec};

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
    } else {
        let mut cleanup = NoTranslateNoMoves::new(tcx);
        cleanup.visit_body(body);
        let _closures = cleanup.closures;
        let _assigns = cleanup_statements(body, &cleanup.unused);
    }
}

/// Whether to suppress borrow checking. We do so for logic or spec functions.
fn should_suppress_borrowck<'tcx>(tcx: TyCtxt<'tcx>, did: DefId) -> bool {
    is_logic(tcx, did) || is_spec(tcx, did) || is_ghost(tcx, did) || is_snapshot(tcx, did)
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

type SpecClosures<'tcx> = HashMap<DefId, IndexVec<FieldIdx, mir::Operand<'tcx>>>;

pub struct NoTranslateNoMoves<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub unused: IndexSet<Local>,
    pub closures: SpecClosures<'tcx>,
}

impl<'tcx> NoTranslateNoMoves<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            unused: IndexSet::new(),
            closures: HashMap::new(),
        }
    }
}

impl<'tcx> MutVisitor<'tcx> for NoTranslateNoMoves<'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }

    fn visit_body(&mut self, body: &mut Body<'tcx>) {
        self.super_body(body);

        self.unused
            .retain(|loc| !body.local_decls[*loc].is_user_variable());
    }

    fn visit_rvalue(&mut self, rvalue: &mut Rvalue<'tcx>, l: Location) {
        match rvalue {
            Rvalue::Aggregate(box AggregateKind::Closure(def_id, _), substs) => {
                if should_suppress_borrowck(self.tcx, *def_id) {
                    for p in substs.iter() {
                        if p.is_move() {
                            let place = p.place().unwrap();
                            if let Some(loc) = place.as_local() {
                                self.unused.insert(loc);
                            }
                        }
                    }
                    self.closures.insert(*def_id, std::mem::take(substs));
                }
            }
            _ => self.super_rvalue(rvalue, l),
        }
    }
}

type SpecAssigns<'tcx> = HashMap<Local, Vec<(mir::Place<'tcx>, Rvalue<'tcx>)>>;

fn cleanup_statements<'tcx>(body: &mut Body<'tcx>, unused: &IndexSet<Local>) -> SpecAssigns<'tcx> {
    let mut assigns: SpecAssigns = HashMap::new();
    for data in body.basic_blocks_mut() {
        data.statements.retain(|statement| match &statement.kind {
            StatementKind::StorageLive(local) | StatementKind::StorageDead(local) => {
                !unused.contains(local)
            }
            StatementKind::PlaceMention(place) => !unused.contains(&place.local),
            StatementKind::Assign(box (place, rvalue)) => {
                let dropped = unused.contains(&place.local);
                if dropped {
                    assigns
                        .entry(place.local)
                        .or_insert(Vec::new())
                        .push((*place, rvalue.clone()));
                }
                !dropped
            }
            StatementKind::FakeRead(box (_, place)) => !unused.contains(&place.local),
            _ => true,
        })
    }
    assigns
}
