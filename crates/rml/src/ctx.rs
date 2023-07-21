use rustc_middle::ty::TyCtxt;

use crate::{
    spec::{collect_hir_specs, HirSpecMap, SpecMap},
    Options,
};

pub struct RmlCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
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

    pub fn get_specs(&self) -> SpecMap<'tcx> {
        SpecMap::new(self.tcx, &self.specs)
    }
}
