use std::fmt;

use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_middle::{
    thir::{self, ExprKind, Thir},
    ty::{self, TyCtxt},
};

use self::util::is_trusted;
use crate::{
    ctx::RmlCtxt,
    error::{Error, RmlErr},
    util::{self, is_internal},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Purity {
    Impure,
    Pure,
    StrictlyPure,
    /// Spec or logic function. May call any pure or impure function.
    Logic,
}

impl Purity {
    pub fn may_call(&self, other: Purity) -> bool {
        use Purity::*;
        match (self, &other) {
            (Logic, StrictlyPure | Pure) => true,
            (Impure, _) => true,
            (Pure, StrictlyPure) => true,
            (p1, p2) if p1 == p2 => true,
            _ => false,
        }
    }

    pub fn is_logic(&self) -> bool {
        matches!(self, Self::Logic)
    }
}

impl fmt::Display for Purity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Purity::Impure => write!(f, "impure"),
            Purity::Pure => write!(f, "pure"),
            Purity::StrictlyPure => write!(f, "strictly pure"),
            Purity::Logic => write!(f, "logic"),
        }
    }
}

impl<'tcx> RmlCtxt<'tcx> {
    pub fn check_purity(&self, did: LocalDefId) {
        let tcx = self.tcx;
        let (thir, expr) = tcx
            .thir_body(did)
            .unwrap_or_else(|_| Error::from(RmlErr).emit(todo!()));
        let thir = thir.borrow();
        if thir.exprs.is_empty() {
            Error::new(tcx.def_span(did), "type checking failed").emit(todo!());
        }

        let did = did.to_def_id();
        // Internal items (e.g., the `exists` and `forall` functions) should not be
        // checked
        if is_internal(tcx, did) || is_trusted(tcx, did) {
            return;
        }
        let purity = get_purity(tcx, did);

        thir::visit::walk_expr(
            &mut PurityVisitor {
                tcx,
                thir: &thir,
                purity,
                did,
            },
            &thir[expr],
        )
    }
}

pub(crate) fn get_purity(tcx: TyCtxt<'_>, did: DefId) -> Purity {
    if util::is_spec(tcx, did) || util::is_logic(tcx, did) {
        Purity::Logic
    } else if util::is_declared_strictly_pure(tcx, did) {
        Purity::StrictlyPure
    } else if util::is_declared_pure(tcx, did) {
        Purity::Pure
    } else {
        Purity::Impure
    }
}

pub(crate) struct PurityVisitor<'a, 'tcx> {
    pub(crate) tcx: TyCtxt<'tcx>,
    pub(crate) thir: &'a Thir<'tcx>,
    pub(crate) purity: Purity,
    pub(crate) did: DefId,
}

impl<'a, 'tcx> thir::visit::Visitor<'a, 'tcx> for PurityVisitor<'a, 'tcx> {
    fn thir(&self) -> &'a Thir<'tcx> {
        self.thir
    }

    fn visit_expr(&mut self, expr: &'a thir::Expr<'tcx>) {
        if let ExprKind::Call { fun, .. } = expr.kind {
            let ty = self.thir[fun].ty.kind();
            match ty {
                &ty::FnDef(func_did, _) => {
                    let called_purity = get_purity(self.tcx, func_did);
                    if !self.purity.may_call(called_purity) {
                        let msg = if called_purity.is_logic() {
                            let name = self.tcx.def_path_str(func_did);
                            let caller = self.tcx.def_path_str(self.did);
                            format!("called logical function '{name}' in program function {caller}")
                        } else {
                            let caller = self.tcx.def_path_str(self.did);
                            format!(
                                "called {} function '{}' from {} function {}",
                                called_purity,
                                self.tcx.def_path_str(func_did),
                                self.purity,
                                caller
                            )
                        };

                        todo!(
                            "Error handling {msg} {:?}",
                            self.tcx.get_attrs_unchecked(self.did)
                        )
                        // self.tcx.sess.span_err_with_code(
                        // self.thir[fun].span,
                        // msg,
                        // rustc_errors::DiagnosticId::Error(String::from("rml"
                        // )), );
                    }
                }
                &ty::FnPtr(..) => {
                    // TODO
                }
                _ => {
                    eprintln!("Encountered error in function {fun:?}");
                    eprintln!("Expr: {expr:?}");
                    eprintln!("Type of callee: {ty:?}");
                    todo!("Why is this an error?")
                }
            }
        }
        thir::visit::walk_expr(self, expr)
    }
}
