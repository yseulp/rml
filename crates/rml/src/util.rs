use rustc_ast::{AttrItem, Attribute};
use rustc_hir::def_id::DefId;
use rustc_middle::ty::TyCtxt;

pub(crate) fn is_logic(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(tcx.get_attrs_unchecked(def_id), &["rml", "decl", "logic"]).is_some()
}

pub(crate) fn is_spec(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(tcx.get_attrs_unchecked(def_id), &["rml", "spec"]).is_some()
}

/// Whether the item has a `#[pure]` attribute. Even if this returns `false`, it might be pure.
pub(crate) fn is_declared_pure(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(tcx.get_attrs_unchecked(def_id), &["rml", "decl", "pure"]).is_some()
}

/// Whether the item has a `#[strictly_pure]` attribute. Even if this returns `false`, it might be strictly pure or pure.
pub(crate) fn is_declared_strictly_pure(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(
        tcx.get_attrs_unchecked(def_id),
        &["rml", "decl", "strictly_pure"],
    )
    .is_some()
}

pub(crate) fn get_attr<'a>(attrs: &'a [Attribute], path: &[&str]) -> Option<&'a AttrItem> {
    for attr in attrs.iter() {
        if attr.is_doc_comment() {
            continue;
        }

        let attr = attr.get_normal_item();

        if attr.path.segments.len() != path.len() {
            continue;
        }

        let matches = attr
            .path
            .segments
            .iter()
            .zip(path.iter())
            .fold(true, |acc, (seg, s)| acc && &*seg.ident.as_str() == *s);

        if matches {
            return Some(attr);
        }
    }
    None
}
