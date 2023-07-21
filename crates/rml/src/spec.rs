use std::collections::HashMap;

pub(crate) use hir::{collect_hir_specs, HirSpecMap};
use rml_syn::SpecKind;
use rustc_hir::{Block, Body, Expr, ExprKind, Local, StmtKind};
use rustc_middle::{hir::map::Map, ty::TyCtxt};
use rustc_span::def_id::DefId;

use crate::term::{HirInto, Term};

use self::hir::{HirSpec, HirSpecCase};

pub(crate) mod hir;

#[derive(Debug, Clone)]
pub struct SpecCase<'hir> {
    pub did: DefId,
    pub kind: SpecKind,
    pub name: String,
    pub pre: Vec<Term<'hir>>,
    pub post: Vec<Term<'hir>>,
    pub variant: Option<Term<'hir>>,
    pub diverges: Term<'hir>,
}

impl<'hir> SpecCase<'hir> {
    pub fn new(
        hir: Map<'hir>,
        hcase: &HirSpecCase,
        normal: &mut u64,
        panic: &mut u64,
    ) -> SpecCase<'hir> {
        let pre = hcase
            .pre
            .iter()
            .map(|did| get_expr_from_did(hir, *did).hir_into(hir))
            .collect();
        let post = hcase
            .post
            .iter()
            .map(|did| get_expr_from_did(hir, *did).hir_into(hir))
            .collect();
        let variant = hcase.variant.map(|did| {
            let body = get_body(hir, did);
            match body.value.kind {
                ExprKind::Block(Block { expr: Some(e), .. }, None) => (*e).hir_into(hir),
                _ => unreachable!(),
            }
        });
        let diverges = get_expr_from_did(hir, hcase.diverges.unwrap()).hir_into(hir);
        let name = if let Some(sym) = hcase.name {
            sym.to_string()
        } else {
            match &hcase.kind {
                SpecKind::Normal => {
                    let n = format!("normal specification case {normal}");
                    *normal += 1;
                    n
                }
                SpecKind::Panic => {
                    let n = format!("panic specification case {panic}");
                    *panic += 1;
                    n
                }
            }
        };

        Self {
            did: hcase.did,
            kind: hcase.kind,
            name,
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
    pub fn new(target: DefId, hir: Map<'hir>, hspec: &HirSpec) -> Spec<'hir> {
        let mut normal_count = 0;
        let mut panic_count = 0;
        Self {
            target,
            cases: hspec
                .cases
                .iter()
                .map(|c| SpecCase::new(hir, c, &mut normal_count, &mut panic_count))
                .collect(),
        }
    }

    pub fn push_case(&mut self, case: SpecCase<'hir>) {
        self.cases.push(case)
    }
}

#[derive(Debug, Clone)]
pub struct SpecMap<'hir>(pub HashMap<DefId, Spec<'hir>>);

impl<'hir> SpecMap<'hir> {
    pub fn new(tcx: TyCtxt<'hir>, hir_smap: &HirSpecMap) -> SpecMap<'hir> {
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
