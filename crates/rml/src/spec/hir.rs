use std::collections::{HashMap, HashSet};

use super::SpecKind;
use rustc_hir::HirId;
use rustc_middle::ty::TyCtxt;
use rustc_span::{def_id::DefId, Symbol};

use crate::util;

#[derive(Debug, Clone)]
pub struct HirFnSpecCase {
    pub did: DefId,
    pub kind: SpecKind,
    pub name: Option<Symbol>,
    pub pre: Vec<DefId>,
    pub post: Vec<DefId>,
    pub variant: Option<DefId>,
    pub diverges: Option<DefId>,
}

impl HirFnSpecCase {
    pub fn new(did: DefId, kind: SpecKind) -> Self {
        Self {
            did,
            kind,
            name: None,
            pre: Default::default(),
            post: Default::default(),
            variant: Default::default(),
            diverges: Default::default(),
        }
    }

    pub fn push_pre(&mut self, did: DefId) {
        self.pre.push(did)
    }

    pub fn push_post(&mut self, did: DefId) {
        self.post.push(did)
    }

    pub fn add_name(&mut self, name: Symbol) {
        debug_assert!(self.name.is_none());
        self.name = Some(name);
    }

    pub fn add_variant(&mut self, did: DefId) {
        debug_assert!(self.variant.is_none());
        self.variant = Some(did);
    }

    pub fn add_diverges(&mut self, did: DefId) {
        debug_assert!(self.diverges.is_none());
        self.diverges = Some(did);
    }
}

#[derive(Debug, Clone)]
pub struct HirFnSpec {
    pub target: DefId,
    pub cases: Vec<HirFnSpecCase>,
}

impl HirFnSpec {
    pub fn new(target: DefId) -> Self {
        Self {
            target,
            cases: Vec::new(),
        }
    }

    pub fn push_case(&mut self, case: HirFnSpecCase) {
        self.cases.push(case)
    }
}

#[derive(Debug)]
pub struct HirItemInvs {
    pub target: DefId,
    pub invariants: Vec<DefId>,
}

impl HirItemInvs {
    pub fn new(target: DefId) -> Self {
        Self {
            target,
            invariants: Vec::new(),
        }
    }

    pub fn push(&mut self, did: DefId) {
        self.invariants.push(did)
    }
}

#[derive(Debug)]
pub struct HirLoopSpec {
    pub target: HirId,
    pub invariants: Vec<DefId>,
    pub modifies: Option<DefId>,
    pub variant: Option<DefId>,
}

#[derive(Debug, Default)]
pub struct HirSpecMap {
    pub fn_specs: HashMap<DefId, HirFnSpec>,
    pub struct_invs: HashMap<DefId, HirItemInvs>,
    pub enum_invs: HashMap<DefId, HirItemInvs>,
    pub trait_invs: HashMap<DefId, HirItemInvs>,
    pub loop_specs: HashMap<HirId, HirLoopSpec>,
}

impl HirSpecMap {
    pub fn insert_fn_spec(&mut self, did: DefId, spec: HirFnSpec) {
        let o = self.fn_specs.insert(did, spec);
        assert!(o.is_none())
    }

    pub fn insert_struct_invs(&mut self, did: DefId, invs: HirItemInvs) {
        let o = self.struct_invs.insert(did, invs);
        assert!(o.is_none())
    }

    pub fn insert_enum_invs(&mut self, did: DefId, invs: HirItemInvs) {
        let o = self.enum_invs.insert(did, invs);
        assert!(o.is_none())
    }

    pub fn insert_trait_invs(&mut self, did: DefId, invs: HirItemInvs) {
        let o = self.trait_invs.insert(did, invs);
        assert!(o.is_none())
    }

    pub fn insert_loop_spec(&mut self, hir_id: HirId, spec: HirLoopSpec) {
        let o = self.loop_specs.insert(hir_id, spec);
        assert!(o.is_none())
    }
}

pub fn collect_hir_specs(tcx: TyCtxt<'_>) -> HirSpecMap {
    // fn specs
    let mut all_spec_case_refs = HashMap::new();
    let mut all_cases = HashMap::new();
    let mut all_pres = HashMap::new();
    let mut all_posts = HashMap::new();
    let mut all_variants = HashMap::new();
    let mut all_diverges = HashMap::new();

    let mut all_structs_with_invs = HashSet::new();
    let mut all_struct_invs = HashMap::new();

    let mut all_enums_with_invs = HashSet::new();
    let mut all_enum_invs = HashMap::new();

    let mut all_traits_with_invs = HashSet::new();
    let mut all_trait_invs = HashMap::new();

    for did in tcx.hir().body_owners() {
        let did = did.to_def_id();

        for attr in tcx.get_attrs_unchecked(did) {
            if util::is_attr(attr, "spec_case_ref") {
                if let std::collections::hash_map::Entry::Vacant(e) = all_spec_case_refs.entry(did)
                {
                    e.insert(vec![attr.value_str().unwrap()]);
                } else {
                    all_spec_case_refs
                        .get_mut(&did)
                        .unwrap()
                        .push(attr.value_str().unwrap());
                }
            } else if util::is_attr(attr, "spec_normal") {
                let name = attr.value_str().unwrap();
                all_cases.insert(name, (did, SpecKind::Normal));
            } else if util::is_attr(attr, "spec_panic") {
                let name = attr.value_str().unwrap();
                all_cases.insert(name, (did, SpecKind::Panic));
            } else if util::is_spec_attr(attr, "pre") {
                let name = attr.value_str().unwrap();
                all_pres.insert(name, did);
            } else if util::is_spec_attr(attr, "post") {
                let name = attr.value_str().unwrap();
                all_posts.insert(name, did);
            } else if util::is_spec_attr(attr, "var") {
                let name = attr.value_str().unwrap();
                all_variants.insert(name, did);
            } else if util::is_spec_attr(attr, "div") {
                let name = attr.value_str().unwrap();
                all_diverges.insert(name, did);
            } else if util::is_spec_attr(attr, "struct_inv") {
                let name = attr.value_str().unwrap();
                all_struct_invs.insert(name, did);
            } else if util::is_spec_attr(attr, "enum_inv") {
                let name = attr.value_str().unwrap();
                all_enum_invs.insert(name, did);
            } else if util::is_spec_attr(attr, "trait_inv") {
                let name = attr.value_str().unwrap();
                all_trait_invs.insert(name, did);
            }
        }
    }

    for id in tcx.hir().items() {
        let did = id.owner_id.to_def_id();
        for attr in tcx.get_attrs_unchecked(did) {
            if util::is_attr(attr, "struct_inv_ref") {
                all_structs_with_invs.insert(did);
            } else if util::is_attr(attr, "enum_inv_ref") {
                all_enums_with_invs.insert(did);
            } else if util::is_attr(attr, "trait_inv_ref") {
                all_traits_with_invs.insert(did);
            }
        }
    }

    let mut map = HirSpecMap::default();
    for (did, syms) in all_spec_case_refs {
        let mut hs = HirFnSpec::new(did);
        for sym in syms {
            let (cid, kind) = all_cases.remove(&sym).unwrap();
            let mut case = HirFnSpecCase::new(cid, kind);
            for attr in tcx.get_attrs_unchecked(cid) {
                if util::is_attr(attr, "spec_part_pre_ref") {
                    let name = attr.value_str().unwrap();
                    case.push_pre(all_pres.remove(&name).unwrap());
                } else if util::is_attr(attr, "spec_part_post_ref") {
                    let name = attr.value_str().unwrap();
                    case.push_post(
                        all_posts
                            .remove(&name)
                            .unwrap_or_else(|| panic!("{}", name.as_str().to_string())),
                    );
                } else if util::is_attr(attr, "spec_part_var_ref") {
                    let name = attr.value_str().unwrap();
                    case.add_variant(all_variants.remove(&name).unwrap());
                } else if util::is_attr(attr, "spec_part_div_ref") {
                    let name = attr.value_str().unwrap();
                    case.add_diverges(all_diverges.remove(&name).unwrap());
                } else if util::is_attr(attr, "spec_name") {
                    let name = attr.value_str().unwrap();
                    case.add_name(name);
                }
            }
            hs.push_case(case);
        }
        map.insert_fn_spec(did, hs)
    }

    for did in all_structs_with_invs {
        let mut invs = HirItemInvs::new(did);
        for attr in tcx.get_attrs_unchecked(did) {
            if util::is_attr(attr, "struct_inv_ref") {
                let name = attr.value_str().unwrap();
                invs.push(all_struct_invs.remove(&name).unwrap());
            }
        }
        map.insert_struct_invs(did, invs);
    }

    for did in all_enums_with_invs {
        let mut invs = HirItemInvs::new(did);
        for attr in tcx.get_attrs_unchecked(did) {
            if util::is_attr(attr, "enum_inv_ref") {
                let name = attr.value_str().unwrap();
                invs.push(all_enum_invs.remove(&name).unwrap());
            }
        }
        map.insert_enum_invs(did, invs);
    }

    for did in all_traits_with_invs {
        let mut invs = HirItemInvs::new(did);
        for attr in tcx.get_attrs_unchecked(did) {
            if util::is_attr(attr, "trait_inv_ref") {
                let name = attr.value_str().unwrap();
                invs.push(all_trait_invs.remove(&name).unwrap());
            }
        }
        map.insert_trait_invs(did, invs);
    }

    map
}
