//! Context for the execution of RML.

use rustc_middle::ty::TyCtxt;

use crate::{
    spec::{collect_hir_specs, HirSpecMap, SpecMap},
    Options,
};

/// The context necessary for RML. Stored between callback phases.
pub struct RmlCtxt<'tcx> {
    /// Reference to the type context.
    pub tcx: TyCtxt<'tcx>,
    /// Options.
    pub(crate) _opts: Options,
    /// All specification cases
    specs: HirSpecMap,
}

impl<'tcx> RmlCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, opts: Options) -> Self {
        Self {
            tcx,
            _opts: opts,
            specs: collect_hir_specs(tcx),
        }
    }

    /// Get specifications.
    pub fn get_specs(&self) -> SpecMap {
        SpecMap::new(self.tcx, &self.specs)
    }
}
