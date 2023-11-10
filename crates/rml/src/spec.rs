use std::collections::HashMap;

pub(crate) use hir::{collect_hir_specs, HirSpecMap};
use rustc_hir::{Block, Body, Expr, ExprKind, HirId, Local, StmtKind};
use rustc_middle::{hir::map::Map, ty::TyCtxt};
use rustc_span::def_id::DefId;
use serde::{Deserialize, Serialize};

use self::hir::{HirFnSpec, HirFnSpecCase, HirItemInvs, HirLoopSpec};
use crate::{
    locset::LocSet,
    term::{
        translation::HirInto,
        wrappers::{DefIdWrapper, HirIdWrapper},
        Term,
    },
};

pub(crate) mod hir;
pub(crate) mod serialize;

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SpecKind {
    Normal,
    Panic,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpecCase {
    pub did: DefIdWrapper,
    pub kind: SpecKind,
    pub name: String,
    pub pre: Vec<Term>,
    pub post: Vec<Term>,
    pub variant: Option<Term>,
    pub diverges: Term,
}

impl<'hir> SpecCase {
    pub fn new(
        hir: Map<'hir>,
        hcase: &HirFnSpecCase,
        normal: &mut u64,
        panic: &mut u64,
    ) -> SpecCase {
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
            did: hcase.did.into(),
            kind: hcase.kind,
            name,
            pre,
            post,
            variant,
            diverges,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FnSpec {
    pub target: DefIdWrapper,
    pub cases: Vec<SpecCase>,
}

impl<'hir> FnSpec {
    pub fn new(target: DefId, hir: Map<'hir>, hspec: &HirFnSpec) -> FnSpec {
        let mut normal_count = 0;
        let mut panic_count = 0;
        Self {
            target: target.into(),
            cases: hspec
                .cases
                .iter()
                .map(|c| SpecCase::new(hir, c, &mut normal_count, &mut panic_count))
                .collect(),
        }
    }

    pub fn push_case(&mut self, case: SpecCase) {
        self.cases.push(case)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ItemInvs {
    pub target: DefIdWrapper,
    pub invariants: Vec<Term>,
}

impl<'hir> ItemInvs {
    pub fn new(target: DefId, hir: Map<'hir>, invs: &HirItemInvs) -> Self {
        Self {
            target: target.into(),
            invariants: invs
                .invariants
                .iter()
                .map(|i| get_expr_from_did(hir, *i).hir_into(hir))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct LoopSpec {
    pub target: HirIdWrapper,
    pub invariants: Vec<Term>,
    pub modifies: Option<LocSet>,
    pub variant: Option<Term>,
}

impl LoopSpec {
    pub fn new<'hir>(target: HirId, hir: Map<'hir>, spec: &HirLoopSpec) -> Self {
        Self {
            target: target.into(),
            invariants: spec
                .invariants
                .iter()
                .map(|did| get_expr_from_did(hir, *did).hir_into(hir))
                .collect(),
            modifies: spec
                .modifies
                .map(|did| get_expr_from_did(hir, did).hir_into(hir)),
            variant: spec
                .variant
                .map(|did| get_return_from_did(hir, did).hir_into(hir)),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct SerializableSpecMap<'s> {
    pub fn_specs: Vec<(DefIdWrapper, &'s FnSpec)>,
    pub struct_invs: Vec<(DefIdWrapper, &'s ItemInvs)>,
    pub enum_invs: Vec<(DefIdWrapper, &'s ItemInvs)>,
    pub trait_invs: Vec<(DefIdWrapper, &'s ItemInvs)>,
    pub loop_specs: Vec<(HirIdWrapper, &'s LoopSpec)>,
}

#[derive(Debug, Clone)]
pub struct SpecMap {
    pub fn_specs: HashMap<DefIdWrapper, FnSpec>,
    pub struct_invs: HashMap<DefIdWrapper, ItemInvs>,
    pub enum_invs: HashMap<DefIdWrapper, ItemInvs>,
    pub trait_invs: HashMap<DefIdWrapper, ItemInvs>,
    pub loop_specs: HashMap<HirIdWrapper, LoopSpec>,
}

impl<'hir> SpecMap {
    pub fn new(tcx: TyCtxt<'hir>, hir_smap: &HirSpecMap) -> SpecMap {
        let hir = tcx.hir();

        let mut fn_specs = HashMap::with_capacity(hir_smap.fn_specs.len());
        for (did, hspec) in &hir_smap.fn_specs {
            let spec = FnSpec::new(*did, hir, hspec);
            let did_w = (*did).into();
            fn_specs.insert(did_w, spec);
        }

        let mut struct_invs = HashMap::with_capacity(hir_smap.struct_invs.len());
        for (did, invs) in &hir_smap.struct_invs {
            let invs = ItemInvs::new(*did, hir, invs);
            let did_w = (*did).into();
            struct_invs.insert(did_w, invs);
        }

        let mut enum_invs = HashMap::with_capacity(hir_smap.enum_invs.len());
        for (did, invs) in &hir_smap.enum_invs {
            let invs = ItemInvs::new(*did, hir, invs);
            let did_w = (*did).into();
            enum_invs.insert(did_w, invs);
        }

        let mut trait_invs = HashMap::with_capacity(hir_smap.trait_invs.len());
        for (did, invs) in &hir_smap.trait_invs {
            let invs = ItemInvs::new(*did, hir, invs);
            let did_w = (*did).into();
            trait_invs.insert(did_w, invs);
        }
        let mut loop_specs = HashMap::with_capacity(hir_smap.loop_specs.len());
        for (hir_id, hspec) in &hir_smap.loop_specs {
            let spec = LoopSpec::new(*hir_id, hir, hspec);
            let hir_id_w = (*hir_id).into();
            loop_specs.insert(hir_id_w, spec);
        }

        Self {
            fn_specs,
            struct_invs,
            enum_invs,
            trait_invs,
            loop_specs,
        }
    }

    pub fn serializable(&self) -> SerializableSpecMap {
        SerializableSpecMap {
            fn_specs: self.fn_specs.iter().map(|(did, s)| (*did, s)).collect(),
            struct_invs: self.struct_invs.iter().map(|(did, i)| (*did, i)).collect(),
            enum_invs: self.enum_invs.iter().map(|(did, i)| (*did, i)).collect(),
            trait_invs: self.trait_invs.iter().map(|(did, i)| (*did, i)).collect(),
            loop_specs: self.loop_specs.iter().map(|(hid, s)| (*hid, s)).collect(),
        }
    }
}

fn get_expr_from_did(hir: Map<'_>, did: DefId) -> &Expr {
    let body = get_body(hir, did);
    match body.value.kind {
        ExprKind::Block(Block { stmts, .. }, None) => match stmts[0].kind {
            StmtKind::Local(Local { init: Some(e), .. }) => e,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn get_return_from_did(hir: Map<'_>, did: DefId) -> &Expr {
    let body = get_body(hir, did);
    match body.value.kind {
        ExprKind::Block(Block { expr: Some(e), .. }, None) => e,
        _ => unreachable!(),
    }
}

fn get_body(hir: Map<'_>, did: DefId) -> &Body<'_> {
    let bid = hir.body_owned_by(did.expect_local());
    hir.body(bid)
}
