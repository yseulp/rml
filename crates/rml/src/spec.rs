pub(crate) mod hir;

use std::collections::HashMap;

pub(crate) use hir::{collect_hir_specs, HirSpecMap};
use rml_syn::SpecKind;
use rustc_hir::{Block, Body, Expr, ExprKind, Local, StmtKind};
use rustc_middle::{hir::map::Map, ty::TyCtxt};
use rustc_span::def_id::DefId;

use self::hir::{HirSpec, HirSpecCase};

#[derive(Debug, Clone)]
pub struct SpecCase<'hir> {
    pub did: DefId,
    pub kind: SpecKind,
    pub pre: Vec<&'hir Expr<'hir>>,
    pub post: Vec<&'hir Expr<'hir>>,
    pub variant: Option<&'hir Expr<'hir>>,
    pub diverges: &'hir Expr<'hir>,
}

impl<'hir> SpecCase<'hir> {
    pub fn new(hir: Map<'hir>, hcase: &HirSpecCase) -> Self {
        let pre = hcase
            .pre
            .iter()
            .map(|did| get_expr_from_did(hir, *did))
            .collect();
        let post = hcase
            .post
            .iter()
            .map(|did| get_expr_from_did(hir, *did))
            .collect();
        let variant = hcase.variant.map(|did| get_expr_from_did(hir, did));
        let diverges = get_expr_from_did(hir, hcase.diverges.unwrap());
        Self {
            did: hcase.did,
            kind: hcase.kind,
            pre,
            post,
            variant,
            diverges,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Spec<'hir> {
    pub target: DefId,
    pub cases: Vec<SpecCase<'hir>>,
}

impl<'hir> Spec<'hir> {
    pub fn new(target: DefId, hir: Map<'hir>, hspec: &HirSpec) -> Self {
        Self {
            target,
            cases: hspec.cases.iter().map(|c| SpecCase::new(hir, c)).collect(),
        }
    }

    pub fn push_case(&mut self, case: SpecCase<'hir>) {
        self.cases.push(case)
    }
}

#[derive(Debug, Clone)]
pub struct SpecMap<'hir>(pub HashMap<DefId, Spec<'hir>>);

impl<'hir> SpecMap<'hir> {
    pub fn new(tcx: TyCtxt<'hir>, hir_smap: &HirSpecMap) -> Self {
        let mut map = HashMap::with_capacity(hir_smap.len());
        let hir = tcx.hir();

        for (did, hspec) in &hir_smap.0 {
            let spec = Spec::new(*did, hir, hspec);

            map.insert(*did, spec);
        }

        Self(map)
    }
}

fn get_expr_from_did<'hir>(hir: Map<'hir>, did: DefId) -> &'hir Expr {
    let body = get_body(hir, did);
    match body.value.kind {
        ExprKind::Block(Block { stmts, .. }, None) => match stmts[0].kind {
            StmtKind::Local(Local { init: Some(e), .. }) => e,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn get_body<'hir>(hir: Map<'hir>, did: DefId) -> &'hir Body<'hir> {
    let bid = hir.body_owned_by(did.expect_local());
    hir.body(bid)
}
