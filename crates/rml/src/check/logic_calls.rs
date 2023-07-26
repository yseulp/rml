use crate::{
    ctx::RmlCtxt,
    error::{Error, RmlErr},
    util,
};
use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_middle::{
    thir::{self, ExprKind, Thir},
    ty::{self, TyCtxt},
};

impl<'tcx> RmlCtxt<'tcx> {
    pub fn check_logic_calls(&self, did: LocalDefId) {
        let tcx = self.tcx;
        let (thir, expr) = tcx
            .thir_body(did)
            .unwrap_or_else(|_| Error::from(RmlErr).emit(tcx.sess));
        let thir = thir.borrow();
        if thir.exprs.is_empty() {
            Error::new(tcx.def_span(did), "type checking failed").emit(tcx.sess);
        }

        let did = did.to_def_id();
        let in_logic_ctx = is_in_logic_ctx(tcx, did);

        thir::visit::walk_expr(
            &mut LogicCallVisitor {
                tcx,
                thir: &thir,
                in_logic_ctx,
            },
            &thir[expr],
        )
    }
}

pub(crate) fn is_in_logic_ctx(tcx: TyCtxt<'_>, did: DefId) -> bool {
    util::is_spec(tcx, did) || util::is_logic(tcx, did)
}

pub(crate) struct LogicCallVisitor<'a, 'tcx> {
    pub(crate) tcx: TyCtxt<'tcx>,
    pub(crate) thir: &'a Thir<'tcx>,
    pub(crate) in_logic_ctx: bool,
}

impl<'a, 'tcx> thir::visit::Visitor<'a, 'tcx> for LogicCallVisitor<'a, 'tcx> {
    fn thir(&self) -> &'a Thir<'tcx> {
        self.thir
    }

    fn visit_expr(&mut self, expr: &thir::Expr<'tcx>) {
        if let ExprKind::Call { fun, .. } = expr.kind {
            if let &ty::FnDef(func_did, _) = self.thir[fun].ty.kind() {
                let called_is_logic = is_in_logic_ctx(self.tcx, func_did);
                if !self.in_logic_ctx && called_is_logic {
                    let name = self.tcx.def_path_str(func_did);
                    let msg = format!("called logical function '{name}' in program function");

                    self.tcx.sess.span_err_with_code(
                        self.thir[fun].span,
                        msg,
                        rustc_errors::DiagnosticId::Error(String::from("rml")),
                    );
                }
            } else {
                todo!("Why is this an error? {fun:?}")
            }
        }

        thir::visit::walk_expr(self, expr)
    }
}
