//! Data structures and functions for collecting, transforming, and storing
//! specifications.

use std::collections::HashMap;

pub(crate) use hir::{collect_hir_specs, HirSpecMap};
use rustc_hir::{Block, Body, Expr, ExprKind, HirId, LetStmt, Param, StmtKind};
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;
use serde::{Deserialize, Serialize};

use self::hir::{HirFnSpec, HirFnSpecCase, HirItemInvs, HirLoopSpec};
use crate::{
    locset::LocSet,
    term::{
        translation::HirInto,
        wrappers::{DefId as TermDefId, HirId as TermHirId},
        Term, TermParam,
    },
};

pub(crate) mod hir;

/// The kind of a function specification.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SpecKind {
    /// Normal execution (no panics).
    Normal,
    /// The function panics.
    Panic,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WithParams<T>
where
    T: std::fmt::Debug + Clone + Serialize,
{
    pub params: Vec<TermParam>,
    pub value: T,
}

impl<T> WithParams<T>
where
    T: std::fmt::Debug + Clone + Serialize,
{
    pub fn new(params: Vec<TermParam>, value: T) -> Self {
        Self { params, value }
    }
}

/// A case of a specification. Extracted from a spec function.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpecCase {
    /// DefId of the spec function.
    pub did: TermDefId,
    /// Kind of the case. Defines the execution.
    pub kind: SpecKind,
    /// Name of the case.
    pub name: String,
    /// Pre-conditions.
    pub pre: Vec<WithParams<Term>>,
    /// Post-conditions.
    pub post: Vec<WithParams<Term>>,
    /// Variant of recursive functions.
    pub variant: Option<WithParams<Term>>,
    /// Condition for diverging execution. I.e., whether the function must
    /// terminate.
    pub diverges: WithParams<Term>,
}

impl<'tcx> SpecCase {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        hcase: &HirFnSpecCase,
        normal: &mut u64,
        panic: &mut u64,
    ) -> SpecCase {
        let pre = hcase
            .pre
            .iter()
            .copied()
            .map(|did| get_params_and_term(did, tcx))
            .collect();
        let post = hcase
            .post
            .iter()
            .copied()
            .map(|did| get_params_and_term(did, tcx))
            .collect();
        let variant = hcase.variant.map(|did| {
            let body = get_body(tcx, did);
            match body.value.kind {
                ExprKind::Block(Block { expr: Some(e), .. }, None) => WithParams::new(
                    (body.params.iter().map(|p| p.hir_into(tcx))).collect(),
                    (*e).hir_into(tcx),
                ),
                _ => unreachable!(),
            }
        });
        let diverges = get_params_and_term(hcase.diverges.unwrap(), tcx);
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

fn get_params_and_term<'tcx>(did: DefId, tcx: TyCtxt<'tcx>) -> WithParams<Term> {
    let (params, e) = get_params_and_expr_from_did(tcx, did);
    let ps: Vec<TermParam> = params.iter().map(|p| p.hir_into(tcx)).collect();
    let t: Term = e.hir_into(tcx);
    WithParams::new(ps, t)
}

/// Complete specification for a function.
#[derive(Debug, Clone, Serialize)]
pub struct FnSpec {
    /// The specified function.
    pub target: TermDefId,
    /// All cases of the specification.
    pub cases: Vec<SpecCase>,
}

impl<'tcx> FnSpec {
    pub fn new(target: DefId, tcx: TyCtxt<'tcx>, hspec: &HirFnSpec) -> FnSpec {
        let mut normal_count = 0;
        let mut panic_count = 0;
        Self {
            target: target.into(),
            cases: hspec
                .cases
                .iter()
                .map(|c| SpecCase::new(tcx, c, &mut normal_count, &mut panic_count))
                .collect(),
        }
    }

    /// Push case `case`.
    pub fn push_case(&mut self, case: SpecCase) {
        self.cases.push(case)
    }
}

/// All invariants for data structure or trait `target`.
#[derive(Debug, Clone, Serialize)]
pub struct ItemInvs {
    /// The item which the invariants describe.
    pub target: TermDefId,
    /// All invariants.
    pub invariants: Vec<WithParams<Term>>,
}

impl<'tcx> ItemInvs {
    pub fn new(target: DefId, tcx: TyCtxt<'tcx>, invs: &HirItemInvs) -> Self {
        Self {
            target: target.into(),
            invariants: invs
                .invariants
                .iter()
                .map(|i| get_params_and_term(*i, tcx))
                .collect(),
        }
    }
}

/// Specification for a loop.
#[derive(Debug, Clone, Serialize)]
pub struct LoopSpec {
    /// The specified loop.
    pub target: TermHirId,
    /// Invariants of the loop.
    pub invariants: Vec<WithParams<Term>>,
    /// What the loop may modify.
    pub modifies: Option<WithParams<LocSet>>,
    /// The variant of the loop.
    pub variant: Option<WithParams<Term>>,
}

impl LoopSpec {
    pub fn new<'tcx>(target: HirId, tcx: TyCtxt<'tcx>, spec: &HirLoopSpec) -> Self {
        Self {
            target: target.into(),
            invariants: spec
                .invariants
                .iter()
                .map(|did| get_params_and_term(*did, tcx))
                .collect(),
            modifies: spec.modifies.map(|did| {
                let (params, e) = get_params_and_expr_from_did(tcx, did);
                WithParams::new(
                    params.iter().map(|p| p.hir_into(tcx)).collect(),
                    e.hir_into(tcx),
                )
            }),
            variant: spec.variant.map(|did| {
                let (params, e) = get_return_from_did(tcx, did);
                WithParams::new(
                    params.iter().map(|p| p.hir_into(tcx)).collect(),
                    e.hir_into(tcx),
                )
            }),
        }
    }
}

/// A serializable collection of all specs collected in the crate.
///
/// Since [SpecMap] contains [HashMap]s which cannot be serialized to JSON---the
/// keys are not strings or numbers---we construct a structure more suitable for
/// serialization.
#[derive(Debug, Clone, Serialize)]
pub struct SerializableSpecMap<'s> {
    /// All specified functions and their specifications.
    pub fn_specs: Vec<SerializableEntry<TermDefId, &'s FnSpec>>,
    /// All specified structs and their specifications.
    pub struct_invs: Vec<SerializableEntry<TermDefId, &'s ItemInvs>>,
    /// All specified enums and their specifications.
    pub enum_invs: Vec<SerializableEntry<TermDefId, &'s ItemInvs>>,
    /// All specified traits and their specifications.
    pub trait_invs: Vec<SerializableEntry<TermDefId, &'s ItemInvs>>,
    /// All specified loops and their specifications.
    pub loop_specs: Vec<SerializableEntry<TermHirId, &'s LoopSpec>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SerializableEntry<K, V> {
    id: K,
    value: V,
}

impl<K, V> SerializableEntry<K, V> {
    pub fn new(id: K, value: V) -> Self {
        Self { id, value }
    }
}

/// All specs collected from the crate.
#[derive(Debug, Clone)]
pub struct SpecMap {
    /// Map from specified functions to their specs.
    pub fn_specs: HashMap<TermDefId, FnSpec>,
    /// Map from specified structs to their invariants.
    pub struct_invs: HashMap<TermDefId, ItemInvs>,
    /// Map from specified enums to their invariants.
    pub enum_invs: HashMap<TermDefId, ItemInvs>,
    /// Map from specified traits to their invariants.
    pub trait_invs: HashMap<TermDefId, ItemInvs>,
    /// Map from specified loops to their invariants.
    pub loop_specs: HashMap<TermHirId, LoopSpec>,
}

impl<'hir> SpecMap {
    pub fn new(tcx: TyCtxt<'hir>, hir_smap: &HirSpecMap) -> SpecMap {
        let mut fn_specs = HashMap::with_capacity(hir_smap.fn_specs.len());
        for (did, hspec) in &hir_smap.fn_specs {
            let spec = FnSpec::new(*did, tcx, hspec);
            let did_w = (*did).into();
            fn_specs.insert(did_w, spec);
        }

        let mut struct_invs = HashMap::with_capacity(hir_smap.struct_invs.len());
        for (did, invs) in &hir_smap.struct_invs {
            let invs = ItemInvs::new(*did, tcx, invs);
            let did_w = (*did).into();
            struct_invs.insert(did_w, invs);
        }

        let mut enum_invs = HashMap::with_capacity(hir_smap.enum_invs.len());
        for (did, invs) in &hir_smap.enum_invs {
            let invs = ItemInvs::new(*did, tcx, invs);
            let did_w = (*did).into();
            enum_invs.insert(did_w, invs);
        }

        let mut trait_invs = HashMap::with_capacity(hir_smap.trait_invs.len());
        for (did, invs) in &hir_smap.trait_invs {
            let invs = ItemInvs::new(*did, tcx, invs);
            let did_w = (*did).into();
            trait_invs.insert(did_w, invs);
        }
        let mut loop_specs = HashMap::with_capacity(hir_smap.loop_specs.len());
        for (hir_id, hspec) in &hir_smap.loop_specs {
            let spec = LoopSpec::new(*hir_id, tcx, hspec);
            let hir_id_w: TermHirId = (*hir_id).into();
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

    /// Make the structure serializable. See [SerializableSpecMap].
    pub fn serializable(&self) -> SerializableSpecMap {
        SerializableSpecMap {
            fn_specs: self
                .fn_specs
                .iter()
                .map(|(did, s)| SerializableEntry::new(*did, s))
                .collect(),
            struct_invs: self
                .struct_invs
                .iter()
                .map(|(did, i)| SerializableEntry::new(*did, i))
                .collect(),
            enum_invs: self
                .enum_invs
                .iter()
                .map(|(did, i)| SerializableEntry::new(*did, i))
                .collect(),
            trait_invs: self
                .trait_invs
                .iter()
                .map(|(did, i)| SerializableEntry::new(*did, i))
                .collect(),
            loop_specs: self
                .loop_specs
                .iter()
                .map(|(hid, s)| SerializableEntry::new(*hid, s))
                .collect(),
        }
    }
}

/// Get the expression from the function at `did`.
fn get_params_and_expr_from_did(tcx: TyCtxt<'_>, did: DefId) -> (&[Param], &Expr) {
    let body = get_body(tcx, did);
    match body.value.kind {
        ExprKind::Block(Block { stmts, .. }, None) => match stmts[0].kind {
            StmtKind::Let(LetStmt { init: Some(e), .. }) => (body.params, e),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

/// Get the returned expression from the function at `did`.
fn get_return_from_did(tcx: TyCtxt<'_>, did: DefId) -> (&[Param], &Expr) {
    let body = get_body(tcx, did);
    match body.value.kind {
        ExprKind::Block(Block { expr: Some(e), .. }, None) => (body.params, e),
        _ => unreachable!(),
    }
}

/// Get the body for function `did`.
fn get_body(tcx: TyCtxt<'_>, did: DefId) -> &Body<'_> {
    tcx.hir_body_owned_by(did.expect_local())
}
