//! Context for the execution of RML.

use rustc_middle::ty::TyCtxt;

use crate::{
    Options,
    hir::{Crate, conversion::convert},
};

/// The context necessary for RML. Stored between callback phases.
pub struct RmlCtxt<'tcx> {
    /// Reference to the type context.
    pub tcx: TyCtxt<'tcx>,
    /// Options.
    pub(crate) _opts: Options,
    /// The converted crate.
    krate: Crate,
}

impl<'tcx> RmlCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, opts: Options) -> Self {
        Self {
            tcx,
            _opts: opts,
            krate: convert(tcx),
        }
    }

    pub fn get_krate(&self) -> &Crate {
        &self.krate
    }
}
