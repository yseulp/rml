use rustc_middle::ty::TyCtxt;

use crate::Options;

pub struct RmlCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub(crate) opts: Options,
}

impl<'tcx> RmlCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, opts: Options) -> Self {
        Self { tcx, opts }
    }
}
