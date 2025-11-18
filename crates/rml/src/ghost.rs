use rustc_middle::ty::TyCtxt;

use crate::{HirInto, hir::expr::Expr, util::is_ghost};

pub(crate) fn get_ghost_expr(tcx: TyCtxt, expr: &rustc_hir::Expr) -> Option<rustc_hir::BodyId> {
    if let rustc_hir::ExprKind::Block(b, None) = expr.kind {
        if b.stmts.len() == 1 && b.expr.is_some() {
            if let rustc_hir::StmtKind::Let(l) = b.stmts[0].kind {
                if matches!(l.pat.kind, rustc_hir::PatKind::Wild)
                    && l.ty.is_none()
                    && l.init.is_some()
                {
                    let e = l.init.unwrap();
                    if let rustc_hir::ExprKind::Block(b, None) = e.kind
                        && b.stmts.is_empty()
                        && let Some(rustc_hir::Expr {
                            kind: rustc_hir::ExprKind::Closure(c),
                            ..
                        }) = b.expr
                    {
                        let did = c.def_id.to_def_id();
                        if is_ghost(tcx, did) {
                            return Some(c.body);
                        }
                    }
                }
            }
        }
    }
    return None;
}

pub(crate) fn convert_ghost_block(tcx: TyCtxt, body_id: rustc_hir::BodyId) -> Expr {
    let body = tcx.hir_body(body_id);
    body.value.hir_into(tcx)
}
