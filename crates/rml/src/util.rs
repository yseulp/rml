use rustc_hir::{AttrItem, Attribute, def_id::DefId};
use rustc_middle::ty::TyCtxt;
use rustc_span::Symbol;

pub fn is_attr(attr: &Attribute, str: &str) -> bool {
    match &attr {
        Attribute::Parsed(_) => false,
        Attribute::Unparsed(item) => {
            let segments = &item.path.segments;
            segments.len() >= 2 && segments[0].as_str() == "rml" && segments[1].as_str() == str
        }
    }
}

pub fn is_spec_attr(attr: &Attribute, str: &str) -> bool {
    match &attr {
        Attribute::Parsed(_) => false,
        Attribute::Unparsed(item) => {
            let segments = &item.path.segments;
            segments.len() >= 3
                && segments[0].as_str() == "rml"
                && segments[1].as_str() == "spec"
                && segments[2].as_str() == str
        }
    }
}

pub fn is_logic(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(tcx.get_attrs_unchecked(def_id), &["rml", "decl", "logic"]).is_some()
}

pub fn is_internal(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(
        tcx.get_attrs_unchecked(def_id),
        &["rml", "decl", "internal"],
    )
    .is_some()
}

pub fn is_trusted(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(tcx.get_attrs_unchecked(def_id), &["rml", "decl", "trusted"]).is_some()
}

pub fn is_spec(tcx: TyCtxt, def_id: DefId) -> bool {
    get_spec_part(tcx.get_attrs_unchecked(def_id)).is_some()
}

/// Whether the item has a `#[pure]` attribute. Even if this returns `false`, it
/// might be pure.
pub fn is_declared_pure(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(tcx.get_attrs_unchecked(def_id), &["rml", "decl", "pure"]).is_some()
}

/// Whether the item has a `#[strictly_pure]` attribute. Even if this returns
/// `false`, it might be strictly pure or pure.
pub fn is_declared_strictly_pure(tcx: TyCtxt, def_id: DefId) -> bool {
    get_attr(
        tcx.get_attrs_unchecked(def_id),
        &["rml", "decl", "strictly_pure"],
    )
    .is_some()
}

pub fn get_spec_part<'a>(attrs: &'a [Attribute]) -> Option<(Symbol, &'a AttrItem)> {
    for attr in attrs.iter() {
        if attr.is_doc_comment() {
            continue;
        }

        let Attribute::Unparsed(attr) = attr else {
            continue;
        };

        if attr.path.segments.len() < 3 {
            continue;
        }

        let segs = &attr.path.segments;

        if segs[0].as_str() == "rml" && segs[1].as_str() == "spec" {
            return Some((segs[2].name, attr));
        }
    }
    None
}

pub fn get_attr<'a>(attrs: &'a [Attribute], path: &[&str]) -> Option<&'a AttrItem> {
    for attr in attrs.iter() {
        if attr.is_doc_comment() {
            continue;
        }

        let Attribute::Unparsed(attr) = attr else {
            continue;
        };

        if attr.path.segments.len() != path.len() {
            continue;
        }

        let matches = attr
            .path
            .segments
            .iter()
            .zip(path.iter())
            .fold(true, |acc, (seg, s)| acc && seg.as_str() == *s);

        if matches {
            return Some(attr);
        }
    }
    None
}
