use std::collections::{HashMap, HashSet};

use rustc_hir::BodyId;
use rustc_middle::ty::TyCtxt;

use super::{
    HirId, ItemKind, Mod, OwnerId, Ty,
    pat::PatKind,
    visit::{Visit, visit_body, visit_expr, visit_item_kind, visit_pat},
};
use crate::{
    HirInto,
    hir::{
        DefId,
        ty::AdtDef,
        visit::{visit_anon_const, visit_closure, visit_const_arg},
    },
};

pub fn extract_extra_info(m: &Mod, tcx: TyCtxt) -> (HashMap<HirId, Ty>, HashMap<DefId, AdtDef>) {
    let mut c = Collector::new(tcx);
    c.visit_mod(m);
    (c.ty_map, c.adt_map)
}

struct Collector<'tcx> {
    ty_map: HashMap<HirId, Ty>,
    adt_map: HashMap<DefId, AdtDef>,
    body_ids: Vec<BodyId>,
    tcx: TyCtxt<'tcx>,
    hir_ids: HashSet<HirId>,
}

impl<'tcx> Collector<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            ty_map: Default::default(),
            adt_map: Default::default(),
            hir_ids: Default::default(),
            body_ids: Vec::with_capacity(2),
        }
    }

    fn push_body_id(&mut self, id: BodyId) {
        self.body_ids.push(id);
    }
}

impl<'a, 'tcx> Visit<'a> for Collector<'tcx> {
    fn visit_item_kind(&mut self, t: &'a super::ItemKind) {
        match t {
            ItemKind::Fn { body_id, .. } => {
                self.push_body_id(*body_id);
            }
            ItemKind::Const { body_id, .. } => self.push_body_id(*body_id),
            _ => (),
        }
        visit_item_kind(self, t)
    }

    fn visit_anon_const(&mut self, t: &'a super::AnonConst) {
        self.push_body_id(t.body_id);
        visit_anon_const(self, t);
    }

    fn visit_body(&mut self, body: &'a super::Body) {
        let Some(id) = self.body_ids.pop() else {
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
            if !self.hir_ids.remove(&hir_id) {
                continue;
            }
            let ty: Ty = ty.hir_into(self.tcx);
            if let Ty::Adt { def, .. } = &ty {
                let def = def.clone();
                self.adt_map.insert(def.did.clone(), def);
            }
            self.ty_map.insert(hir_id, ty);
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

    fn visit_closure(&mut self, t: &'a super::expr::Closure) {
        self.push_body_id(t.body_id);
        visit_closure(self, t);
    }

    fn visit_const_arg(&mut self, t: &'a super::ConstArg) {
        self.hir_ids.insert(t.hir_id.clone());
        visit_const_arg(self, t);
    }
}
