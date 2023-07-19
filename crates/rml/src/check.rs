use crate::ctx::RmlCtxt;

pub(crate) mod logic_calls;
pub(crate) mod purity;

impl<'tcx> RmlCtxt<'tcx> {
    pub fn validate(&mut self) {
        for did in self.tcx.hir().body_owners() {
            self.check_purity(did);
            self.check_logic_calls(did);
        }
    }
}
