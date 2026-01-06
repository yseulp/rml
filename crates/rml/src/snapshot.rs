use rustc_middle::ty::TyCtxt;

use crate::{
    HirInto,
    hir::expr::{Expr, ExprKind},
    util::is_snapshot,
};

pub(crate) fn get_snapshot_expr(tcx: TyCtxt, expr: &rustc_hir::Expr) -> Option<rustc_hir::BodyId> {
    if let rustc_hir::ExprKind::Call(path_expr, args) = expr.kind {
        if let rustc_hir::ExprKind::Path(ref qpath) = path_expr.kind
            && let rustc_hir::QPath::Resolved(None, p) = qpath
            && let Some(did) = p.res.opt_def_id()
        {
            if tcx.def_path_str(did).contains("snapshot_from_fn") {
                if let Some(arg) = args.get(0) {
                    if let rustc_hir::ExprKind::Closure(c) = arg.kind {
                        let did = c.def_id.to_def_id();
                        if is_snapshot(tcx, did) {
                            return Some(c.body);
                        }
                    }
                }
            }
        }
    }
    None
}

pub(crate) fn convert_snapshot(tcx: TyCtxt, body_id: rustc_hir::BodyId) -> Expr {
    let body = tcx.hir_body(body_id);
    let mut e: Expr = body.value.hir_into(tcx);

    let ExprKind::Block { block, .. } = *e.kind else {
        panic!("Expected snapshot")
    };

    e.kind = Box::new(ExprKind::Snapshot {
        expr: block.expr.unwrap(),
    });
    e
}
