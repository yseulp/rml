use std::collections::{HashMap, HashSet};

use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;

use super::{
    HirId, ItemKind, Mod, OwnerId, Ty,
    pat::PatKind,
    visit::{Visit, visit_body, visit_expr, visit_item_kind, visit_pat},
};
use crate::HirInto;

pub fn extract_types(m: &Mod, tcx: TyCtxt) -> HashMap<HirId, Ty> {
    let mut c = Collector::new(tcx);
    c.visit_mod(m);
    c.map
}

struct Collector<'tcx> {
    map: HashMap<HirId, Ty>,
    last_body_id: Option<BodyId>,
    tcx: TyCtxt<'tcx>,
    hir_ids: HashSet<HirId>,
}

impl<'tcx> Collector<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            map: Default::default(),
            hir_ids: Default::default(),
            last_body_id: None,
        }
    }
}

impl<'a, 'tcx> Visit<'a> for Collector<'tcx> {
    fn visit_item_kind(&mut self, t: &'a super::ItemKind) {
        match t {
            ItemKind::Fn { body_id, .. } => {
                self.last_body_id = Some(body_id.clone());
            }
            ItemKind::Const { body_id, .. } => self.last_body_id = Some(body_id.clone()),
            _ => (),
        }
        visit_item_kind(self, t)
    }

    fn visit_body(&mut self, body: &'a super::Body) {
        let Some(id) = self.last_body_id.take() else {
            panic!("Encountered body {body:?} but no id was set");
        };

        visit_body(self, body);

        let owner: OwnerId = id.hir_id.owner.into();

        let res = self.tcx.typeck_body(id);

        for (lid, ty) in res.node_types().items_in_stable_order() {
            let hir_id = HirId {
                owner: owner.clone(),
                local_id: lid.into(),
            };
            if !self.hir_ids.contains(&hir_id) {
                continue;
            }
            let ty: Ty = ty.hir_into(self.tcx);
            self.map.insert(hir_id, ty);
        }
    }

    fn visit_expr(&mut self, t: &'a super::expr::Expr) {
        self.hir_ids.insert(t.hir_id.clone());
        visit_expr(self, t);
    }

    fn visit_pat(&mut self, t: &'a super::pat::Pat) {
        match t.kind.as_ref() {
            PatKind::Binding { hir_id, .. } => {
                self.hir_ids.insert(hir_id.clone());
            }
            _ => {}
        }

        visit_pat(self, t);
    }
}
