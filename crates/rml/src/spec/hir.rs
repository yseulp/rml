use std::collections::HashMap;

use super::SpecKind;
use rustc_middle::ty::TyCtxt;
use rustc_span::{def_id::DefId, Symbol};

use crate::util;

#[derive(Debug, Clone)]
pub struct HirSpecCase {
    pub did: DefId,
    pub kind: SpecKind,
    pub name: Option<Symbol>,
    pub pre: Vec<DefId>,
    pub post: Vec<DefId>,
    pub variant: Option<DefId>,
    pub diverges: Option<DefId>,
}

impl HirSpecCase {
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
pub struct HirSpec {
    pub target: DefId,
    pub cases: Vec<HirSpecCase>,
}

impl HirSpec {
    pub fn new(target: DefId) -> Self {
        Self {
            target,
            cases: Vec::new(),
        }
    }

    pub fn push_case(&mut self, case: HirSpecCase) {
        self.cases.push(case)
    }
}

#[derive(Debug, Default)]
pub struct HirSpecMap(pub HashMap<DefId, HirSpec>);

impl HirSpecMap {
    pub fn insert(&mut self, did: DefId, spec: HirSpec) {
        let o = self.0.insert(did, spec);
        assert!(o.is_none())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

pub fn collect_hir_specs(tcx: TyCtxt<'_>) -> HirSpecMap {
    let mut all_spec_case_refs = HashMap::new();
    let mut all_cases = HashMap::new();
    let mut all_pres = HashMap::new();
    let mut all_posts = HashMap::new();
    let mut all_variants = HashMap::new();
    let mut all_diverges = HashMap::new();

    for did in tcx.hir().body_owners() {
        let did = did.to_def_id();

        for attr in tcx.get_attrs_unchecked(did) {
            if util::is_attr(attr, "spec_case_ref") {
                if let std::collections::hash_map::Entry::Vacant(e) = all_spec_case_refs.entry(did) {
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
            } else if util::is_spec_part(attr, "pre") {
                let name = attr.value_str().unwrap();
                all_pres.insert(name, did);
            } else if util::is_spec_part(attr, "post") {
                let name = attr.value_str().unwrap();
                all_posts.insert(name, did);
            } else if util::is_spec_part(attr, "var") {
                let name = attr.value_str().unwrap();
                all_variants.insert(name, did);
            } else if util::is_spec_part(attr, "div") {
                let name = attr.value_str().unwrap();
                all_diverges.insert(name, did);
            }
        }
    }

    let mut map = HirSpecMap::default();
    for (did, syms) in all_spec_case_refs {
        let mut hs = HirSpec::new(did);
        for sym in syms {
            let (cid, kind) = all_cases.remove(&sym).unwrap();
            let mut case = HirSpecCase::new(cid, kind);
            for attr in tcx.get_attrs_unchecked(cid) {
                if util::is_attr(attr, "spec_part_pre_ref") {
                    let name = attr.value_str().unwrap();
                    case.push_pre(all_pres.remove(&name).unwrap());
                } else if util::is_attr(attr, "spec_part_post_ref") {
                    let name = attr.value_str().unwrap();
                    case.push_post(all_posts.remove(&name).unwrap_or_else(|| { panic!("{}", name.as_str().to_string()) }));
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
        map.insert(did, hs)
    }

    map
}
