use crate::ctx::RmlCtxt;

pub(crate) mod purity;

impl<'tcx> RmlCtxt<'tcx> {
    pub fn validate(&mut self) {
        for did in self.tcx.hir_body_owners() {
            self.check_purity(did);
        }
    }
}
