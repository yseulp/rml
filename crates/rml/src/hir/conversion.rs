use std::{cell::RefCell, collections::HashMap, num::NonZero, ops::Deref};

use Ctor;
use Def;
use rustc_hir as hir;
use rustc_middle::ty::TyCtxt;
use rustc_span::{
    BytePos as HirBytePos, Span as HirSpan, Symbol as HirSymbol,
    def_id::{DefIndex as HirDefIndex, LocalDefId as HirLocalDefId},
    symbol::Ident as HirIdent,
};
use type_extract::extract_extra_info;

use super::*;
use crate::{
    FromHir, HirInto,
    ghost::{convert_ghost_block, get_ghost_expr},
    spec::{SpecMap, collect_hir_specs},
    util::{get_attr, is_spec},
};

thread_local! {static SPEC_MAP: RefCell<Option<SpecMap>> = RefCell::new(None);}

pub fn convert(tcx: TyCtxt<'_>) -> Crate {
    SPEC_MAP.with(|smap| {
        *smap.borrow_mut() = Some(SpecMap::new(tcx, &collect_hir_specs(tcx)));
        let m = tcx.hir_root_module();
        let top_mod = m.hir_into(tcx);
        let (types, adts) = extract_extra_info(&top_mod, tcx);
        let types = types.into_iter().map(Into::into).collect();
        let adts = adts.into_iter().map(Into::into).collect();
        Crate {
            top_mod,
            types,
            adts,
        }
    })
}

impl From<rustc_hir::ItemId> for ItemId {
    fn from(value: rustc_hir::ItemId) -> Self {
        Self {
            owner_id: value.owner_id.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Mod<'hir>> for Mod {
    fn from_hir(value: &'hir hir::Mod<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Mod {
            spans: value.spans.into(),
            items: value
                .item_ids
                .iter()
                .filter(|id| {
                    let did = id.owner_id.to_def_id();
                    let is_spec_fn = is_spec(tcx, did);
                    let is_spec_const =
                        get_attr(tcx.get_attrs_unchecked(did), &["rml", "spec_part_div_ref"])
                            .is_some();
                    !(is_spec_fn || is_spec_const)
                })
                .map(|id| {
                    let mut item: Item = tcx.hir_item(*id).hir_into(tcx);
                    SPEC_MAP.with_borrow_mut(|r| {
                        let smap = r.as_mut().unwrap();
                        match &mut item.kind {
                            ItemKind::Fn { spec_cases, .. } => {
                                if let Some(spec) = smap.fn_specs.remove(&DefId {
                                    index: id.owner_id.def_id.local_def_index.into(),
                                    krate: CrateNum(0),
                                }) {
                                    *spec_cases = spec.cases;
                                }
                            }
                            _ => {}
                        }
                    });
                    item
                })
                .collect(),
        }
    }
}

impl From<hir::ModSpans> for ModSpans {
    fn from(value: hir::ModSpans) -> Self {
        ModSpans {
            inner_span: value.inner_span.into(),
            inject_use_span: value.inject_use_span.into(),
        }
    }
}

impl From<HirSpan> for Span {
    fn from(value: HirSpan) -> Self {
        Span {
            lo: value.lo().into(),
            hi: value.hi().into(),
            // ctxt: value.ctxt().into(),
            parent: value.parent().map(|p| p.into()),
        }
    }
}

impl From<HirBytePos> for BytePos {
    fn from(value: HirBytePos) -> Self {
        BytePos(value.0)
    }
}

impl From<HirLocalDefId> for LocalDefId {
    fn from(value: HirLocalDefId) -> Self {
        LocalDefId {
            local_def_index: value.local_def_index.into(),
        }
    }
}

impl From<HirDefIndex> for DefIndex {
    fn from(value: HirDefIndex) -> Self {
        DefIndex(value.as_u32())
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Item<'hir>> for Item {
    fn from_hir(value: &'hir hir::Item<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Item {
            owner_id: value.owner_id.into(),
            kind: (&value.kind).hir_into(tcx),
            span: value.span.into(),
            vis_span: value.vis_span.into(),
        }
    }
}

impl From<HirIdent> for Ident {
    fn from(value: HirIdent) -> Self {
        Ident {
            name: value.name.into(),
            span: value.span.into(),
        }
    }
}

impl From<HirSymbol> for Symbol {
    fn from(value: HirSymbol) -> Self {
        Symbol(value.to_ident_string())
    }
}

impl From<&HirSymbol> for Symbol {
    fn from(value: &HirSymbol) -> Self {
        Symbol(value.to_ident_string())
    }
}

impl From<hir::OwnerId> for OwnerId {
    fn from(value: hir::OwnerId) -> Self {
        OwnerId {
            def_id: value.def_id.into(),
        }
    }
}

impl<'hir> FromHir<'hir, Vec<hir::def::Res>> for Vec<Res> {
    fn from_hir(value: Vec<hir::def::Res>, tcx: TyCtxt<'hir>) -> Self {
        let mut res = Vec::with_capacity(value.len());
        for i in value {
            res.push(i.hir_into(tcx));
        }
        res
    }
}

impl<'hir> FromHir<'hir, hir::def::Res> for Res {
    fn from_hir(value: hir::def::Res, _: TyCtxt<'hir>) -> Self {
        match value {
            rustc_hir::def::Res::Def(def_kind, def_id) => Self::Def {
                def: Def {
                    kind: (&def_kind).into(),
                    id: (&def_id).into(),
                },
            },
            rustc_hir::def::Res::PrimTy(prim_ty) => Self::PrimTy {
                ty: (&prim_ty).into(),
            },
            rustc_hir::def::Res::SelfTyParam { trait_ } => Self::SelfTyParam {
                trait_: (&trait_).into(),
            },
            rustc_hir::def::Res::SelfTyAlias {
                alias_to,
                forbid_generic,
                is_trait_impl,
            } => Self::SelfTyAlias {
                alias_to: (&alias_to).into(),
                forbid_generic,
                is_trait_impl,
            },
            rustc_hir::def::Res::SelfCtor(def_id) => Self::SelfCtor((&def_id).into()),
            rustc_hir::def::Res::Local(id) => Self::Local { id: id.into() },
            rustc_hir::def::Res::ToolMod => Self::ToolMod,
            rustc_hir::def::Res::NonMacroAttr(non_macro_attr_kind) => {
                Self::NonMacroAttr((&non_macro_attr_kind).into())
            }
            rustc_hir::def::Res::Err => Self::Err,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::ItemKind<'hir>> for ItemKind {
    fn from_hir(value: &'hir hir::ItemKind<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::ItemKind::ExternCrate(symbol, ident) => ItemKind::ExternCrate {
                symbol: symbol.map(|s| s.into()),
                ident: (*ident).into(),
            },
            hir::ItemKind::Use(path, use_kind) => ItemKind::Use {
                path: Path {
                    span: path.span.into(),
                    res: path.res.clone().into_vec().hir_into(tcx),
                    segments: path.segments.iter().map(|s| s.hir_into(tcx)).collect(),
                },
                use_kind: use_kind.into(),
            },
            hir::ItemKind::Static(ident, hir_ty, m, body) => Self::Static {
                ident: (*ident).into(),
                ty: (*hir_ty).hir_into(tcx),
                r#const: matches!(m, hir::Mutability::Mut),
                body: tcx.hir_body(*body).hir_into(tcx),
            },
            hir::ItemKind::Const(ident, hir_ty, generics, body) => Self::Const {
                ident: (*ident).into(),
                ty: (*hir_ty).hir_into(tcx),
                generics: (*generics).hir_into(tcx),
                body: tcx.hir_body(*body).hir_into(tcx),
                body_id: *body,
            },
            hir::ItemKind::Fn {
                ident,
                sig,
                generics,
                body,
                ..
            } => Self::Fn {
                ident: (*ident).into(),
                sig: sig.hir_into(tcx),
                generics: (*generics).hir_into(tcx),
                body_id: body.clone(),
                body: tcx.hir_body(*body).hir_into(tcx),
                spec_cases: vec![],
            },
            hir::ItemKind::Mod(ident, m) => Self::Mod {
                ident: (*ident).into(),
                r#mod: (*m).hir_into(tcx),
            },
            hir::ItemKind::TyAlias(ident, hir_ty, generics) => Self::TyAlias {
                ident: (*ident).into(),
                ty: (*hir_ty).hir_into(tcx),
                generics: (*generics).hir_into(tcx),
            },
            hir::ItemKind::Enum(ident, enum_def, generics) => Self::Enum {
                ident: (*ident).into(),
                def: enum_def.hir_into(tcx),
                generics: (*generics).hir_into(tcx),
            },
            hir::ItemKind::Struct(ident, data, generics) => Self::Struct {
                ident: (*ident).into(),
                data: data.hir_into(tcx),
                generics: (*generics).hir_into(tcx),
            },
            hir::ItemKind::Union(ident, data, generics) => Self::Union {
                ident: (*ident).into(),
                data: data.hir_into(tcx),
                generics: (*generics).hir_into(tcx),
            },
            hir::ItemKind::Trait(auto, safe, ident, generics, bounds, refs) => Self::Trait {
                ident: (*ident).into(),
                is_auto: matches!(auto, hir::IsAuto::Yes),
                is_safe: matches!(safe, hir::Safety::Safe),
                generics: (*generics).hir_into(tcx),
                bounds: (*bounds).hir_into(tcx),
                refs: refs.iter().map(Into::into).collect(),
            },
            hir::ItemKind::TraitAlias(ident, generics, bounds) => Self::TraitAlias {
                ident: (*ident).into(),
                generics: (*generics).hir_into(tcx),
                bounds: (*bounds).hir_into(tcx),
            },
            hir::ItemKind::Impl(i) => Self::Impl {
                r#impl: (*i).hir_into(tcx),
            },
            hir::ItemKind::Macro(ident, _, kind) => Self::Macro {
                ident: (*ident).into(),
                def: MacroDef {},
                kind: match kind {
                    rustc_span::MacroKind::Bang => MacroKind::Bang,
                    rustc_span::MacroKind::Attr => MacroKind::Attr,
                    rustc_span::MacroKind::Derive => MacroKind::Derive,
                },
            },
            hir::ItemKind::ForeignMod { .. } => Self::ForeignMod,
            hir::ItemKind::GlobalAsm { .. } => Self::GlobalAsm,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::FnSig<'hir>> for FnSig {
    fn from_hir(value: &'hir hir::FnSig<'hir>, tcx: TyCtxt<'hir>) -> Self {
        FnSig {
            header: value.header.into(),
            decl: value.decl.hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl From<hir::FnHeader> for FnHeader {
    fn from(value: hir::FnHeader) -> Self {
        FnHeader {
            safety: matches!(value.safety, hir::HeaderSafety::Normal(hir::Safety::Safe)),
            // TODO: handled other case
            constness: matches!(value.constness, hir::Constness::Const),
            asyncness: matches!(value.asyncness, hir::IsAsync::Async(_)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::EnumDef<'hir>> for EnumDef {
    fn from_hir(value: &'hir hir::EnumDef<'hir>, tcx: TyCtxt<'hir>) -> Self {
        EnumDef {
            variants: value.variants.hir_into(tcx),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Variant<'hir>> for Variant {
    fn from_hir(value: &'hir hir::Variant<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Variant {
            ident: value.ident.into(),
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            data: (&value.data).hir_into(tcx),
            disr_expr: value.disr_expr.map(|e| e.hir_into(tcx)),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::VariantData<'hir>> for VariantData {
    fn from_hir(value: &'hir hir::VariantData<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::VariantData::Struct { fields, recovered } => Self::Struct {
                fields: (*fields).hir_into(tcx),
                recovered: matches!(recovered, rustc_ast::Recovered::Yes(_)),
            },
            hir::VariantData::Tuple(fs, hir_id, lid) => Self::Tuple {
                def: (*fs).hir_into(tcx),
                hir_id: hir_id.into(),
                local_def_id: (*lid).into(),
            },
            hir::VariantData::Unit(hir_id, lid) => Self::Unit {
                hir_id: hir_id.into(),
                local_def_id: (*lid).into(),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::FieldDef<'hir>> for FieldDef {
    fn from_hir(value: &'hir hir::FieldDef<'hir>, tcx: TyCtxt<'hir>) -> Self {
        FieldDef {
            span: value.span.into(),
            vis_span: value.vis_span.into(),
            ident: value.ident.into(),
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            ty: value.ty.hir_into(tcx),
        }
    }
}

impl From<&hir::TraitItemRef> for TraitItemRef {
    fn from(value: &hir::TraitItemRef) -> Self {
        TraitItemRef {
            id: value.id.into(),
            ident: value.ident.into(),
            kind: value.kind.into(),
            span: value.span.into(),
        }
    }
}

impl From<hir::TraitItemId> for TraitItemId {
    fn from(value: hir::TraitItemId) -> Self {
        TraitItemId {
            owner_id: value.owner_id.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Impl<'hir>> for Impl {
    fn from_hir(value: &'hir hir::Impl<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Impl {
            constness: matches!(value.constness, hir::Constness::Const),
            safety: matches!(value.safety, hir::Safety::Safe),
            polarity: value.polarity.into(),
            defaultness: value.defaultness.into(),
            defaultness_span: value.defaultness_span.map(Into::into),
            generics: value.generics.hir_into(tcx),
            of_trait: value.of_trait.as_ref().map(|t| (t).hir_into(tcx)),
            self_ty: value.self_ty.hir_into(tcx),
            items: value.items.hir_into(tcx),
        }
    }
}

impl From<hir::ImplPolarity> for ImplPolarity {
    fn from(value: hir::ImplPolarity) -> Self {
        match value {
            hir::ImplPolarity::Negative(sp) => Self::Negative { span: sp.into() },
            _ => Self::Positive,
        }
    }
}

impl From<hir::Defaultness> for Defaultness {
    fn from(value: hir::Defaultness) -> Self {
        match value {
            hir::Defaultness::Default { has_value } => Self::Default { has_value },
            _ => Self::Final,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Generics<'hir>> for Generics {
    fn from_hir(value: &'hir hir::Generics<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Generics {
            params: value.params.hir_into(tcx),
            predicates: value.predicates.hir_into(tcx),
            has_where_clause_predicates: value.has_where_clause_predicates,
            where_clause_span: value.where_clause_span.into(),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::WherePredicate<'hir>> for WherePredicate {
    fn from_hir(value: &'hir hir::WherePredicate<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            kind: value.kind.hir_into(tcx),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::WherePredicateKind<'hir>> for WherePredicateKind {
    fn from_hir(value: &'hir hir::WherePredicateKind<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::WherePredicateKind::BoundPredicate(b) => Self::Bound(b.hir_into(tcx)),
            hir::WherePredicateKind::RegionPredicate(r) => Self::Region(r.hir_into(tcx)),
            hir::WherePredicateKind::EqPredicate(e) => Self::Eq(e.hir_into(tcx)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::WhereBoundPredicate<'hir>> for WhereBoundPredicate {
    fn from_hir(value: &'hir hir::WhereBoundPredicate<'hir>, tcx: TyCtxt<'hir>) -> Self {
        WhereBoundPredicate {
            origin: value.origin.into(),
            bound_generic_params: value.bound_generic_params.hir_into(tcx),
            bounded_ty: value.bounded_ty.hir_into(tcx),
            bounds: value.bounds.hir_into(tcx),
        }
    }
}

impl From<hir::PredicateOrigin> for PredicateOrigin {
    fn from(value: hir::PredicateOrigin) -> Self {
        match value {
            hir::PredicateOrigin::WhereClause => Self::WhereClause,
            hir::PredicateOrigin::GenericParam => Self::GenericParam,
            hir::PredicateOrigin::ImplTrait => Self::ImplTrait,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::WhereRegionPredicate<'hir>> for WhereRegionPredicate {
    fn from_hir(value: &'hir hir::WhereRegionPredicate<'hir>, tcx: TyCtxt<'hir>) -> Self {
        WhereRegionPredicate {
            in_where_clause: value.in_where_clause,
            lifetime: value.lifetime.into(),
            bounds: value.bounds.hir_into(tcx),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::WhereEqPredicate<'hir>> for WhereEqPredicate {
    fn from_hir(value: &'hir hir::WhereEqPredicate<'hir>, tcx: TyCtxt<'hir>) -> Self {
        WhereEqPredicate {
            lhs_ty: value.lhs_ty.hir_into(tcx),
            rhs_ty: value.rhs_ty.hir_into(tcx),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::ImplItemRef> for ImplItemRef {
    fn from_hir(value: &'hir hir::ImplItemRef, _: TyCtxt<'hir>) -> Self {
        ImplItemRef {
            id: value.id.into(),
            ident: value.ident.into(),
            kind: value.kind.into(),
            span: value.span.into(),
            trait_item_def_id: value.trait_item_def_id.map(|d| (&d).into()),
        }
    }
}

impl From<hir::ImplItemId> for ImplItemId {
    fn from(value: hir::ImplItemId) -> Self {
        ImplItemId {
            owner_id: value.owner_id.into(),
        }
    }
}

impl From<hir::AssocItemKind> for AssocItemKind {
    fn from(value: hir::AssocItemKind) -> Self {
        match value {
            hir::AssocItemKind::Const => Self::Const,
            hir::AssocItemKind::Fn { has_self } => Self::Fn { has_self },
            hir::AssocItemKind::Type => Self::Type,
        }
    }
}

impl<'hir, R, T> FromHir<'hir, &'hir hir::Path<'hir, R>> for Path<T>
where
    T: FromHir<'hir, &'hir R>,
    R: std::fmt::Debug,
{
    fn from_hir(value: &'hir hir::Path<'hir, R>, tcx: TyCtxt<'hir>) -> Self {
        Path {
            span: value.span.into(),
            res: (&value.res).hir_into(tcx),
            segments: value.segments.iter().map(|s| s.hir_into(tcx)).collect(),
        }
    }
}

impl From<&hir::UseKind> for UseKind {
    fn from(value: &hir::UseKind) -> Self {
        match value {
            hir::UseKind::Single(ident) => UseKind::Single {
                ident: (*ident).into(),
            },
            hir::UseKind::Glob => UseKind::Glob,
            hir::UseKind::ListStem => UseKind::ListStem,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::PathSegment<'hir>> for PathSegment {
    fn from_hir(value: &'hir hir::PathSegment<'hir>, tcx: TyCtxt<'hir>) -> Self {
        PathSegment {
            ident: value.ident.into(),
            hir_id: value.hir_id.into(),
            res: value.res.into(),
            args: value.args.map(|a| a.hir_into(tcx)),
            infer_args: value.infer_args,
        }
    }
}

impl From<hir::HirId> for HirId {
    fn from(value: hir::HirId) -> Self {
        (&value).into()
    }
}

impl From<&hir::HirId> for HirId {
    fn from(value: &hir::HirId) -> Self {
        HirId {
            owner: value.owner.into(),
            local_id: value.local_id.into(),
        }
    }
}

impl From<hir::ItemLocalId> for ItemLocalId {
    fn from(value: hir::ItemLocalId) -> Self {
        ItemLocalId(value.as_u32())
    }
}

impl From<hir::def::Res> for Res {
    fn from(value: hir::def::Res) -> Self {
        match &value {
            hir::def::Res::Def(kind, id) => Self::Def {
                def: Def {
                    kind: kind.into(),
                    id: id.into(),
                },
            },
            hir::def::Res::PrimTy(ty) => Self::PrimTy { ty: ty.into() },
            hir::def::Res::SelfTyParam { trait_ } => Self::SelfTyParam {
                trait_: trait_.into(),
            },
            hir::def::Res::SelfTyAlias {
                alias_to,
                forbid_generic,
                is_trait_impl,
            } => Self::SelfTyAlias {
                alias_to: alias_to.into(),
                forbid_generic: *forbid_generic,
                is_trait_impl: *is_trait_impl,
            },
            hir::def::Res::SelfCtor(id) => Self::SelfCtor(id.into()),
            hir::def::Res::Local(id) => Self::Local { id: id.into() },
            hir::def::Res::ToolMod => Self::ToolMod,
            hir::def::Res::NonMacroAttr(kind) => Self::NonMacroAttr(kind.into()),
            hir::def::Res::Err => Self::Err,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::GenericArgs<'hir>> for GenericArgs {
    fn from_hir(value: &'hir hir::GenericArgs<'hir>, tcx: TyCtxt<'hir>) -> Self {
        GenericArgs {
            args: value.args.hir_into(tcx),
            constraints: value.constraints.hir_into(tcx),
            parenthesized: value.parenthesized.into(),
            span_ext: value.span_ext.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::GenericArg<'hir>> for GenericArg {
    fn from_hir(value: &'hir hir::GenericArg<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::GenericArg::Lifetime(l) => Self::Lifetime {
                lifetime: (*l).into(),
            },
            hir::GenericArg::Type(ty) => Self::Type {
                ty: (*ty).hir_into(tcx),
            },
            hir::GenericArg::Const(c) => Self::Const {
                c: (*c).hir_into(tcx),
            },
            hir::GenericArg::Infer(i) => Self::Infer { infer: i.into() },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::AssocItemConstraint<'hir>> for AssocItemConstraint {
    fn from_hir(value: &'hir hir::AssocItemConstraint<'hir>, tcx: TyCtxt<'hir>) -> Self {
        AssocItemConstraint {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            gen_args: value.gen_args.hir_into(tcx),
            kind: (&value.kind).hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::AssocItemConstraintKind<'hir>> for AssocItemConstraintKind {
    fn from_hir(value: &'hir hir::AssocItemConstraintKind<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::AssocItemConstraintKind::Equality { term } => AssocItemConstraintKind::Equality {
                term: term.hir_into(tcx),
            },
            hir::AssocItemConstraintKind::Bound { bounds } => AssocItemConstraintKind::Bound {
                bounds: (*bounds).hir_into(tcx),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Term<'hir>> for Term {
    fn from_hir(value: &'hir hir::Term<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::Term::Ty(ty) => Self::Ty {
                ty: (*ty).hir_into(tcx),
            },
            hir::Term::Const(c) => Self::Const {
                c: (*c).hir_into(tcx),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::GenericBound<'hir>> for GenericBound {
    fn from_hir(value: &'hir hir::GenericBound<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::GenericBound::Trait(r) => GenericBound::Trait {
                trait_ref: r.hir_into(tcx),
            },
            hir::GenericBound::Outlives(l) => GenericBound::Outlives {
                lifetime: (*l).into(),
            },
            hir::GenericBound::Use(args, sp) => Self::Use {
                args: (*args).hir_into(tcx),
                span: (*sp).into(),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::PreciseCapturingArg<'hir>> for PreciseCapturingArg {
    fn from_hir(value: &'hir hir::PreciseCapturingArg<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::PreciseCapturingArg::Lifetime(l) => Self::Lifetime {
                lifetime: (*l).into(),
            },
            hir::PreciseCapturingArg::Param(a) => Self::Param {
                arg: a.hir_into(tcx),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::PreciseCapturingNonLifetimeArg>
    for PreciseCapturingNonLifetimeArg
{
    fn from_hir(value: &'hir hir::PreciseCapturingNonLifetimeArg, tcx: TyCtxt<'hir>) -> Self {
        PreciseCapturingNonLifetimeArg {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            res: (&value.res).hir_into(tcx),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::PolyTraitRef<'hir>> for PolyTraitRef {
    fn from_hir(value: &'hir hir::PolyTraitRef<'hir>, tcx: TyCtxt<'hir>) -> Self {
        PolyTraitRef {
            bound_generic_params: value.bound_generic_params.hir_into(tcx),
            trait_ref: (&value.trait_ref).hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::TraitRef<'hir>> for TraitRef {
    fn from_hir(value: &'hir hir::TraitRef<'hir>, tcx: TyCtxt<'hir>) -> Self {
        TraitRef {
            path: value.path.hir_into(tcx),
            hir_ref_id: value.hir_ref_id.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::GenericParam<'hir>> for GenericParam {
    fn from_hir(value: &'hir hir::GenericParam<'hir>, tcx: TyCtxt<'hir>) -> Self {
        GenericParam {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            name: value.name.into(),
            span: value.span.into(),
            pure_wrt_drop: value.pure_wrt_drop,
            kind: (&value.kind).hir_into(tcx),
            colon_span: value.colon_span.map(Into::into),
            source: value.source.into(),
        }
    }
}

impl From<hir::ParamName> for ParamName {
    fn from(value: hir::ParamName) -> Self {
        match value {
            hir::ParamName::Plain(ident) => Self::Plain {
                ident: ident.into(),
            },
            hir::ParamName::Fresh => Self::Fresh,
            hir::ParamName::Error(..) => Self::Error,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::GenericParamKind<'hir>> for GenericParamKind {
    fn from_hir(value: &'hir hir::GenericParamKind<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::GenericParamKind::Lifetime { kind } => Self::Lifetime { kind: kind.into() },
            hir::GenericParamKind::Type { default, synthetic } => Self::Type {
                default: default.map(|ty| ty.hir_into(tcx)),
                synthetic: *synthetic,
            },
            hir::GenericParamKind::Const {
                ty,
                default,
                synthetic,
            } => Self::Const {
                ty: (*ty).hir_into(tcx),
                default: default.map(|d| d.hir_into(tcx)),
                synthetic: *synthetic,
            },
        }
    }
}

impl From<&hir::LifetimeParamKind> for LifetimeParamKind {
    fn from(value: &hir::LifetimeParamKind) -> Self {
        match value {
            hir::LifetimeParamKind::Explicit => Self::Explicit,
            hir::LifetimeParamKind::Elided(kind) => Self::Elided { kind: kind.into() },
            hir::LifetimeParamKind::Error => Self::Error,
        }
    }
}

impl From<&hir::MissingLifetimeKind> for MissingLifetimeKind {
    fn from(value: &hir::MissingLifetimeKind) -> Self {
        match value {
            hir::MissingLifetimeKind::Underscore => Self::Underscore,
            hir::MissingLifetimeKind::Ampersand => Self::Ampersand,
            hir::MissingLifetimeKind::Comma => Self::Comma,
            hir::MissingLifetimeKind::Brackets => Self::Brackets,
        }
    }
}

impl<'hir, A> FromHir<'hir, &'hir hir::ConstArg<'hir, A>> for ConstArg {
    fn from_hir(value: &'hir hir::ConstArg<'hir, A>, tcx: TyCtxt<'hir>) -> Self {
        ConstArg {
            hir_id: value.hir_id.into(),
            kind: (&value.kind).hir_into(tcx),
        }
    }
}

impl<'hir, A> FromHir<'hir, &'hir hir::ConstArgKind<'hir, A>> for ConstArgKind {
    fn from_hir(value: &'hir hir::ConstArgKind<'hir, A>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::ConstArgKind::Path(qpath) => Self::Path {
                path: qpath.hir_into(tcx),
            },
            hir::ConstArgKind::Anon(anon_const) => Self::Anon {
                ac: (*anon_const).hir_into(tcx),
            },
            rustc_hir::ConstArgKind::Infer(span, _) => Self::Infer {
                span: (*span).into(),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::QPath<'hir>> for QPath {
    fn from_hir(value: &'hir hir::QPath<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::QPath::Resolved(ty, p) => Self::Resolved {
                ty: ty.map(|t| t.hir_into(tcx)),
                path: (*p).hir_into(tcx),
            },
            hir::QPath::TypeRelative(ty, seg) => Self::TypeRelative {
                ty: (*ty).hir_into(tcx),
                seg: (*seg).hir_into(tcx),
            },
            hir::QPath::LangItem(li, sp) => Self::LangItem {
                item: li.into(),
                span: (*sp).into(),
            },
        }
    }
}

impl From<&hir::LangItem> for LangItem {
    fn from(value: &hir::LangItem) -> Self {
        match value {
            hir::LangItem::Sized => Self::Sized,
            hir::LangItem::Unsize => Self::Unsize,
            hir::LangItem::StructuralPeq => Self::StructuralPeq,
            hir::LangItem::Copy => Self::Copy,
            hir::LangItem::Clone => Self::Clone,
            hir::LangItem::CloneFn => Self::CloneFn,
            hir::LangItem::Sync => Self::Sync,
            hir::LangItem::DiscriminantKind => Self::DiscriminantKind,
            hir::LangItem::Discriminant => Self::Discriminant,
            hir::LangItem::PointeeTrait => Self::PointeeTrait,
            hir::LangItem::Metadata => Self::Metadata,
            hir::LangItem::DynMetadata => Self::DynMetadata,
            hir::LangItem::Freeze => Self::Freeze,
            hir::LangItem::FnPtrTrait => Self::FnPtrTrait,
            hir::LangItem::FnPtrAddr => Self::FnPtrAddr,
            hir::LangItem::Drop => Self::Drop,
            hir::LangItem::Destruct => Self::Destruct,
            hir::LangItem::AsyncDrop => Self::AsyncDrop,
            hir::LangItem::AsyncDropInPlace => Self::AsyncDropInPlace,
            hir::LangItem::CoerceUnsized => Self::CoerceUnsized,
            hir::LangItem::DispatchFromDyn => Self::DispatchFromDyn,
            hir::LangItem::TransmuteOpts => Self::TransmuteOpts,
            hir::LangItem::TransmuteTrait => Self::TransmuteTrait,
            hir::LangItem::Add => Self::Add,
            hir::LangItem::Sub => Self::Sub,
            hir::LangItem::Mul => Self::Mul,
            hir::LangItem::Div => Self::Div,
            hir::LangItem::Rem => Self::Rem,
            hir::LangItem::Neg => Self::Neg,
            hir::LangItem::Not => Self::Not,
            hir::LangItem::BitXor => Self::BitXor,
            hir::LangItem::BitAnd => Self::BitAnd,
            hir::LangItem::BitOr => Self::BitOr,
            hir::LangItem::Shl => Self::Shl,
            hir::LangItem::Shr => Self::Shr,
            hir::LangItem::AddAssign => Self::AddAssign,
            hir::LangItem::SubAssign => Self::SubAssign,
            hir::LangItem::MulAssign => Self::MulAssign,
            hir::LangItem::DivAssign => Self::DivAssign,
            hir::LangItem::RemAssign => Self::RemAssign,
            hir::LangItem::BitXorAssign => Self::BitXorAssign,
            hir::LangItem::BitAndAssign => Self::BitAndAssign,
            hir::LangItem::BitOrAssign => Self::BitOrAssign,
            hir::LangItem::ShlAssign => Self::ShlAssign,
            hir::LangItem::ShrAssign => Self::ShrAssign,
            hir::LangItem::Index => Self::Index,
            hir::LangItem::IndexMut => Self::IndexMut,
            hir::LangItem::UnsafeCell => Self::UnsafeCell,
            hir::LangItem::VaList => Self::VaList,
            hir::LangItem::Deref => Self::Deref,
            hir::LangItem::DerefMut => Self::DerefMut,
            hir::LangItem::DerefPure => Self::DerefPure,
            hir::LangItem::DerefTarget => Self::DerefTarget,
            hir::LangItem::Receiver => Self::Receiver,
            hir::LangItem::Fn => Self::Fn,
            hir::LangItem::FnMut => Self::FnMut,
            hir::LangItem::FnOnce => Self::FnOnce,
            hir::LangItem::AsyncFn => Self::AsyncFn,
            hir::LangItem::AsyncFnMut => Self::AsyncFnMut,
            hir::LangItem::AsyncFnOnce => Self::AsyncFnOnce,
            hir::LangItem::AsyncFnOnceOutput => Self::AsyncFnOnceOutput,
            hir::LangItem::CallOnceFuture => Self::CallOnceFuture,
            hir::LangItem::CallRefFuture => Self::CallRefFuture,
            hir::LangItem::AsyncFnKindHelper => Self::AsyncFnKindHelper,
            hir::LangItem::AsyncFnKindUpvars => Self::AsyncFnKindUpvars,
            hir::LangItem::FnOnceOutput => Self::FnOnceOutput,
            hir::LangItem::Iterator => Self::Iterator,
            hir::LangItem::FusedIterator => Self::FusedIterator,
            hir::LangItem::Future => Self::Future,
            hir::LangItem::FutureOutput => Self::FutureOutput,
            hir::LangItem::AsyncIterator => Self::AsyncIterator,
            hir::LangItem::CoroutineState => Self::CoroutineState,
            hir::LangItem::Coroutine => Self::Coroutine,
            hir::LangItem::CoroutineReturn => Self::CoroutineReturn,
            hir::LangItem::CoroutineYield => Self::CoroutineYield,
            hir::LangItem::CoroutineResume => Self::CoroutineResume,
            hir::LangItem::Unpin => Self::Unpin,
            hir::LangItem::Pin => Self::Pin,
            hir::LangItem::OrderingEnum => Self::OrderingEnum,
            hir::LangItem::PartialEq => Self::PartialEq,
            hir::LangItem::PartialOrd => Self::PartialOrd,
            hir::LangItem::CVoid => Self::CVoid,
            hir::LangItem::Panic => Self::Panic,
            hir::LangItem::PanicNounwind => Self::PanicNounwind,
            hir::LangItem::PanicFmt => Self::PanicFmt,
            hir::LangItem::ConstPanicFmt => Self::ConstPanicFmt,
            hir::LangItem::PanicBoundsCheck => Self::PanicBoundsCheck,
            hir::LangItem::PanicMisalignedPointerDereference => {
                Self::PanicMisalignedPointerDereference
            }
            hir::LangItem::PanicInfo => Self::PanicInfo,
            hir::LangItem::PanicLocation => Self::PanicLocation,
            hir::LangItem::PanicImpl => Self::PanicImpl,
            hir::LangItem::PanicCannotUnwind => Self::PanicCannotUnwind,
            hir::LangItem::PanicInCleanup => Self::PanicInCleanup,
            hir::LangItem::PanicAddOverflow => Self::PanicAddOverflow,
            hir::LangItem::PanicSubOverflow => Self::PanicSubOverflow,
            hir::LangItem::PanicMulOverflow => Self::PanicMulOverflow,
            hir::LangItem::PanicDivOverflow => Self::PanicDivOverflow,
            hir::LangItem::PanicRemOverflow => Self::PanicRemOverflow,
            hir::LangItem::PanicNegOverflow => Self::PanicNegOverflow,
            hir::LangItem::PanicShrOverflow => Self::PanicShrOverflow,
            hir::LangItem::PanicShlOverflow => Self::PanicShlOverflow,
            hir::LangItem::PanicDivZero => Self::PanicDivZero,
            hir::LangItem::PanicRemZero => Self::PanicRemZero,
            hir::LangItem::PanicCoroutineResumed => Self::PanicCoroutineResumed,
            hir::LangItem::PanicAsyncFnResumed => Self::PanicAsyncFnResumed,
            hir::LangItem::PanicAsyncGenFnResumed => Self::PanicAsyncGenFnResumed,
            hir::LangItem::PanicGenFnNone => Self::PanicGenFnNone,
            hir::LangItem::PanicCoroutineResumedPanic => Self::PanicCoroutineResumedPanic,
            hir::LangItem::PanicAsyncFnResumedPanic => Self::PanicAsyncFnResumedPanic,
            hir::LangItem::PanicAsyncGenFnResumedPanic => Self::PanicAsyncGenFnResumedPanic,
            hir::LangItem::PanicGenFnNonePanic => Self::PanicGenFnNonePanic,
            hir::LangItem::BeginPanic => Self::BeginPanic,
            hir::LangItem::FormatArgument => Self::FormatArgument,
            hir::LangItem::FormatArguments => Self::FormatArguments,
            hir::LangItem::FormatCount => Self::FormatCount,
            hir::LangItem::FormatPlaceholder => Self::FormatPlaceholder,
            hir::LangItem::FormatUnsafeArg => Self::FormatUnsafeArg,
            hir::LangItem::ExchangeMalloc => Self::ExchangeMalloc,
            hir::LangItem::DropInPlace => Self::DropInPlace,
            hir::LangItem::AllocLayout => Self::AllocLayout,
            hir::LangItem::Start => Self::Start,
            hir::LangItem::EhPersonality => Self::EhPersonality,
            hir::LangItem::EhCatchTypeinfo => Self::EhCatchTypeinfo,
            hir::LangItem::OwnedBox => Self::OwnedBox,
            hir::LangItem::GlobalAlloc => Self::GlobalAlloc,
            hir::LangItem::PtrUnique => Self::PtrUnique,
            hir::LangItem::PhantomData => Self::PhantomData,
            hir::LangItem::ManuallyDrop => Self::ManuallyDrop,
            hir::LangItem::MaybeUninit => Self::MaybeUninit,
            hir::LangItem::Termination => Self::Termination,
            hir::LangItem::Try => Self::Try,
            hir::LangItem::Tuple => Self::Tuple,
            hir::LangItem::SliceLen => Self::SliceLen,
            hir::LangItem::TryTraitFromResidual => Self::TryTraitFromResidual,
            hir::LangItem::TryTraitFromOutput => Self::TryTraitFromOutput,
            hir::LangItem::TryTraitBranch => Self::TryTraitBranch,
            hir::LangItem::TryTraitFromYeet => Self::TryTraitFromYeet,
            hir::LangItem::PointerLike => Self::PointerLike,
            hir::LangItem::ConstParamTy => Self::ConstParamTy,
            hir::LangItem::UnsizedConstParamTy => Self::UnsizedConstParamTy,
            hir::LangItem::Poll => Self::Poll,
            hir::LangItem::PollReady => Self::PollReady,
            hir::LangItem::PollPending => Self::PollPending,
            hir::LangItem::AsyncGenReady => Self::AsyncGenReady,
            hir::LangItem::AsyncGenPending => Self::AsyncGenPending,
            hir::LangItem::AsyncGenFinished => Self::AsyncGenFinished,
            hir::LangItem::ResumeTy => Self::ResumeTy,
            hir::LangItem::GetContext => Self::GetContext,
            hir::LangItem::Context => Self::Context,
            hir::LangItem::FuturePoll => Self::FuturePoll,
            hir::LangItem::AsyncIteratorPollNext => Self::AsyncIteratorPollNext,
            hir::LangItem::IntoAsyncIterIntoIter => Self::IntoAsyncIterIntoIter,
            hir::LangItem::Option => Self::Option,
            hir::LangItem::OptionSome => Self::OptionSome,
            hir::LangItem::OptionNone => Self::OptionNone,
            hir::LangItem::ResultOk => Self::ResultOk,
            hir::LangItem::ResultErr => Self::ResultErr,
            hir::LangItem::ControlFlowContinue => Self::ControlFlowContinue,
            hir::LangItem::ControlFlowBreak => Self::ControlFlowBreak,
            hir::LangItem::IntoFutureIntoFuture => Self::IntoFutureIntoFuture,
            hir::LangItem::IntoIterIntoIter => Self::IntoIterIntoIter,
            hir::LangItem::IteratorNext => Self::IteratorNext,
            hir::LangItem::PinNewUnchecked => Self::PinNewUnchecked,
            hir::LangItem::RangeFrom => Self::RangeFrom,
            hir::LangItem::RangeFull => Self::RangeFull,
            hir::LangItem::RangeInclusiveStruct => Self::RangeInclusiveStruct,
            hir::LangItem::RangeInclusiveNew => Self::RangeInclusiveNew,
            hir::LangItem::Range => Self::Range,
            hir::LangItem::RangeToInclusive => Self::RangeToInclusive,
            hir::LangItem::RangeTo => Self::RangeTo,
            hir::LangItem::String => Self::String,
            hir::LangItem::CStr => Self::CStr,
            hir::LangItem::UseCloned => Self::UseCloned,
            hir::LangItem::UnsafeUnpin => Self::UnsafeUnpin,
            hir::LangItem::UnsafePinned => Self::UnsafePinned,
            hir::LangItem::ReceiverTarget => Self::ReceiverTarget,
            hir::LangItem::LegacyReceiver => Self::LegacyReceiver,
            hir::LangItem::PanicNullPointerDereference => Self::PanicNullPointerDereference,
            hir::LangItem::PanicCoroutineResumedDrop => Self::PanicCoroutineResumedDrop,
            hir::LangItem::PanicAsyncFnResumedDrop => Self::PanicAsyncFnResumedDrop,
            hir::LangItem::PanicAsyncGenFnResumedDrop => Self::PanicAsyncGenFnResumedDrop,
            hir::LangItem::PanicGenFnNoneDrop => Self::PanicGenFnNoneDrop,
            hir::LangItem::BikeshedGuaranteedNoDrop => Self::BikeshedGuaranteedNoDrop,
            hir::LangItem::CoercePointeeValidated => Self::CoercePointeeValidated,
            hir::LangItem::RangeMax => Self::RangeMax,
            hir::LangItem::RangeMin => Self::RangeMin,
            hir::LangItem::RangeSub => Self::RangeSub,
            hir::LangItem::RangeFromCopy => Self::RangeFromCopy,
            hir::LangItem::RangeCopy => Self::RangeCopy,
            hir::LangItem::RangeInclusiveCopy => Self::RangeInclusiveCopy,
            hir::LangItem::ContractBuildCheckEnsures => Self::ContractBuildCheckEnsures,
            hir::LangItem::ContractCheckRequires => Self::ContractCheckRequires,
            hir::LangItem::DefaultTrait4 => Self::DefaultTrait4,
            hir::LangItem::DefaultTrait3 => Self::DefaultTrait3,
            hir::LangItem::DefaultTrait2 => Self::DefaultTrait2,
            hir::LangItem::DefaultTrait1 => Self::DefaultTrait1,
            hir::LangItem::ContractCheckEnsures => Self::ContractCheckEnsures,
        }
    }
}

impl<'hir, A> FromHir<'hir, &'hir hir::Ty<'hir, A>> for HirTy {
    fn from_hir(value: &'hir hir::Ty<'hir, A>, tcx: TyCtxt<'hir>) -> Self {
        HirTy {
            hir_id: value.hir_id.into(),
            kind: Box::new((&value.kind).hir_into(tcx)),
            span: value.span.into(),
        }
    }
}

impl<'hir, A> FromHir<'hir, &'hir hir::TyKind<'hir, A>> for HirTyKind {
    fn from_hir(value: &'hir hir::TyKind<'hir, A>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::TyKind::InferDelegation(id, kind) => Self::InferDelegation {
                def_id: id.into(),
                kind: kind.into(),
            },
            hir::TyKind::Slice(ty) => Self::Slice {
                ty: (*ty).hir_into(tcx),
            },
            hir::TyKind::Array(ty, l) => Self::Array {
                ty: (*ty).hir_into(tcx),
                len: (*l).hir_into(tcx),
            },
            hir::TyKind::Ptr(ty) => Self::Ptr {
                ty: ty.hir_into(tcx),
            },
            hir::TyKind::Ref(l, ty) => Self::Ref {
                lifetime: (*l).into(),
                ty: ty.hir_into(tcx),
            },
            hir::TyKind::BareFn(ty) => Self::BareFn {
                ty: (*ty).hir_into(tcx),
            },
            hir::TyKind::Never => Self::Never,
            hir::TyKind::Tup(tys) => Self::Tup {
                tys: (*tys).hir_into(tcx),
            },
            hir::TyKind::Path(path) => Self::Path {
                path: path.hir_into(tcx),
            },
            hir::TyKind::OpaqueDef(..) => Self::OpaqueDef,
            hir::TyKind::TraitObject(ts, tr) => Self::TraitObject {
                refs: ts.iter().map(|r| r.hir_into(tcx)).collect(),
                lifetime: (tr.pointer()).into(),
                syntax: (&tr.tag()).into(),
            },
            hir::TyKind::Typeof(c) => Self::Typeof {
                r#const: (*c).hir_into(tcx),
            },
            hir::TyKind::Infer(_) => Self::Infer,
            hir::TyKind::Err(_) => Self::Err,
            hir::TyKind::Pat(ty, p) => Self::Pat {
                ty: (*ty).hir_into(tcx),
                pat: (*p).hir_into(tcx),
            },
            hir::TyKind::UnsafeBinder(..) => todo!("TyKind::UnsafeBinder"),
            hir::TyKind::TraitAscription(..) => todo!("TyKind::TraitAscription"),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx hir::TyPat<'tcx>> for TyPat {
    fn from_hir(value: &'tcx hir::TyPat<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: value.kind.hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, hir::TyPatKind<'tcx>> for TyPatKind {
    fn from_hir(value: hir::TyPatKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            rustc_hir::TyPatKind::Range(const_arg, const_arg1) => Self::Range {
                start: const_arg.hir_into(tcx),
                end: const_arg1.hir_into(tcx),
            },
            rustc_hir::TyPatKind::Or(ty_pats) => Self::Or {
                pats: ty_pats.iter().map(|p| p.hir_into(tcx)).collect(),
            },
            rustc_hir::TyPatKind::Err(_) => Self::Err,
        }
    }
}

impl From<&hir::InferDelegationKind> for InferDelegationKind {
    fn from(value: &hir::InferDelegationKind) -> Self {
        match value {
            hir::InferDelegationKind::Output => Self::Output,
            hir::InferDelegationKind::Input(n) => Self::Input { id: *n },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::MutTy<'hir>> for MutHirTy {
    fn from_hir(value: &'hir hir::MutTy<'hir>, tcx: TyCtxt<'hir>) -> Self {
        MutHirTy {
            ty: value.ty.hir_into(tcx),
            mutbl: matches!(value.mutbl, hir::Mutability::Mut),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::BareFnTy<'hir>> for BareFnHirTy {
    fn from_hir(value: &'hir hir::BareFnTy<'hir>, tcx: TyCtxt<'hir>) -> Self {
        BareFnHirTy {
            safety: matches!(value.safety, hir::Safety::Safe),
            generic_params: value.generic_params.hir_into(tcx),
            decl: value.decl.hir_into(tcx),
            param_idents: value
                .param_idents
                .iter()
                .copied()
                .map(|o| o.map(Into::into))
                .collect(),
        }
    }
}

impl From<&rustc_ast::TraitObjectSyntax> for TraitObjectSyntax {
    fn from(value: &rustc_ast::TraitObjectSyntax) -> Self {
        match value {
            rustc_ast::TraitObjectSyntax::Dyn => Self::Dyn,
            rustc_ast::TraitObjectSyntax::DynStar => Self::DynStar,
            rustc_ast::TraitObjectSyntax::None => Self::None,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::def::Res> for Res {
    fn from_hir(value: &'hir hir::def::Res, _: TyCtxt<'hir>) -> Self {
        match value {
            hir::def::Res::Def(kind, id) => Self::Def {
                def: Def {
                    kind: kind.into(),
                    id: id.into(),
                },
            },
            hir::def::Res::PrimTy(ty) => Self::PrimTy { ty: ty.into() },
            hir::def::Res::SelfTyParam { trait_ } => Self::SelfTyParam {
                trait_: trait_.into(),
            },
            hir::def::Res::SelfTyAlias {
                alias_to,
                forbid_generic,
                is_trait_impl,
            } => Self::SelfTyAlias {
                alias_to: alias_to.into(),
                forbid_generic: *forbid_generic,
                is_trait_impl: *is_trait_impl,
            },
            hir::def::Res::SelfCtor(id) => Self::SelfCtor(id.into()),
            hir::def::Res::Local(id) => Self::Local { id: id.into() },
            hir::def::Res::ToolMod => Self::ToolMod,
            hir::def::Res::NonMacroAttr(kind) => Self::NonMacroAttr(kind.into()),
            hir::def::Res::Err => Self::Err,
        }
    }
}

impl From<&hir::def::DefKind> for DefKind {
    fn from(value: &hir::def::DefKind) -> Self {
        match value {
            hir::def::DefKind::Mod => Self::Mod,
            hir::def::DefKind::Struct => Self::Struct,
            hir::def::DefKind::Union => Self::Union,
            hir::def::DefKind::Enum => Self::Enum,
            hir::def::DefKind::Variant => Self::Variant,
            hir::def::DefKind::Trait => Self::Trait,
            hir::def::DefKind::TyAlias => Self::TyAlias,
            hir::def::DefKind::ForeignTy => Self::ForeignTy,
            hir::def::DefKind::TraitAlias => Self::TraitAlias,
            hir::def::DefKind::AssocTy => Self::AssocTy,
            hir::def::DefKind::TyParam => Self::TyParam,
            hir::def::DefKind::Fn => Self::Fn,
            hir::def::DefKind::Const => Self::Const,
            hir::def::DefKind::ConstParam => Self::ConstParam,
            hir::def::DefKind::Static {
                safety: hir::Safety::Safe,
                mutability: hir::Mutability::Mut,
                nested,
            } => Self::Static {
                safety: true,
                mutability: true,
                nested: *nested,
            },
            hir::def::DefKind::Static {
                safety: hir::Safety::Safe,
                mutability: hir::Mutability::Not,
                nested,
            } => Self::Static {
                safety: true,
                mutability: false,
                nested: *nested,
            },
            hir::def::DefKind::Static {
                safety: hir::Safety::Unsafe,
                mutability: hir::Mutability::Mut,
                nested,
            } => Self::Static {
                safety: false,
                mutability: true,
                nested: *nested,
            },
            hir::def::DefKind::Static {
                safety: hir::Safety::Unsafe,
                mutability: hir::Mutability::Not,
                nested,
            } => Self::Static {
                safety: false,
                mutability: false,
                nested: *nested,
            },
            hir::def::DefKind::Ctor(of, hir::def::CtorKind::Fn) => Self::Ctor {
                ctor: Ctor(of.into(), true),
            },
            hir::def::DefKind::Ctor(of, hir::def::CtorKind::Const) => Self::Ctor {
                ctor: Ctor(of.into(), false),
            },
            hir::def::DefKind::AssocFn => Self::AssocFn,
            hir::def::DefKind::AssocConst => Self::AssocConst,
            hir::def::DefKind::Macro(kind) => Self::Macro { kind: kind.into() },
            hir::def::DefKind::ExternCrate => Self::ExternCrate,
            hir::def::DefKind::Use => Self::Use,
            hir::def::DefKind::ForeignMod => Self::ForeignMod,
            hir::def::DefKind::AnonConst => Self::AnonConst,
            hir::def::DefKind::InlineConst => Self::InlineConst,
            hir::def::DefKind::OpaqueTy => Self::OpaqueTy,
            hir::def::DefKind::Field => Self::Field,
            hir::def::DefKind::LifetimeParam => Self::LifetimeParam,
            hir::def::DefKind::GlobalAsm => Self::GlobalAsm,
            hir::def::DefKind::Impl { of_trait } => Self::Impl {
                of_trait: *of_trait,
            },
            hir::def::DefKind::Closure => Self::Closure,
            hir::def::DefKind::SyntheticCoroutineBody => Self::SyntheticCoroutineBody,
        }
    }
}

impl From<&hir::def::CtorOf> for CtorOf {
    fn from(value: &hir::def::CtorOf) -> Self {
        match value {
            hir::def::CtorOf::Struct => Self::Struct,
            hir::def::CtorOf::Variant => Self::Variant,
        }
    }
}

impl From<&rustc_span::MacroKind> for MacroKind {
    fn from(value: &rustc_span::MacroKind) -> Self {
        match value {
            rustc_span::MacroKind::Bang => Self::Bang,
            rustc_span::MacroKind::Attr => Self::Attr,
            rustc_span::MacroKind::Derive => Self::Derive,
        }
    }
}

impl From<&hir::PrimTy> for PrimHirTy {
    fn from(value: &hir::PrimTy) -> Self {
        match value {
            hir::PrimTy::Int(i) => Self::Int { ty: i.into() },
            hir::PrimTy::Uint(i) => Self::Uint { ty: i.into() },
            hir::PrimTy::Float(f) => Self::Float { ty: f.into() },
            hir::PrimTy::Str => Self::Str,
            hir::PrimTy::Bool => Self::Bool,
            hir::PrimTy::Char => Self::Char,
        }
    }
}

impl From<&rustc_ast::IntTy> for IntTy {
    fn from(value: &rustc_ast::IntTy) -> Self {
        match value {
            rustc_ast::IntTy::Isize => Self::Isize,
            rustc_ast::IntTy::I8 => Self::I8,
            rustc_ast::IntTy::I16 => Self::I16,
            rustc_ast::IntTy::I32 => Self::I32,
            rustc_ast::IntTy::I64 => Self::I64,
            rustc_ast::IntTy::I128 => Self::I128,
        }
    }
}

impl From<&rustc_ast::UintTy> for UintTy {
    fn from(value: &rustc_ast::UintTy) -> Self {
        match value {
            rustc_ast::UintTy::Usize => Self::Usize,
            rustc_ast::UintTy::U8 => Self::U8,
            rustc_ast::UintTy::U16 => Self::U16,
            rustc_ast::UintTy::U32 => Self::U32,
            rustc_ast::UintTy::U64 => Self::U64,
            rustc_ast::UintTy::U128 => Self::U128,
        }
    }
}

impl From<&rustc_ast::FloatTy> for FloatTy {
    fn from(value: &rustc_ast::FloatTy) -> Self {
        match value {
            rustc_ast::FloatTy::F16 => Self::F16,
            rustc_ast::FloatTy::F32 => Self::F32,
            rustc_ast::FloatTy::F64 => Self::F64,
            rustc_ast::FloatTy::F128 => Self::F128,
        }
    }
}

impl From<&rustc_span::def_id::DefId> for DefId {
    fn from(value: &rustc_span::def_id::DefId) -> Self {
        DefId {
            index: value.index.into(),
            krate: (&value.krate).into(),
        }
    }
}

impl From<&rustc_span::def_id::CrateNum> for CrateNum {
    fn from(value: &rustc_span::def_id::CrateNum) -> Self {
        CrateNum(value.as_u32())
    }
}

impl From<&hir::def::NonMacroAttrKind> for NonMacroAttrKind {
    fn from(value: &hir::def::NonMacroAttrKind) -> Self {
        match value {
            hir::def::NonMacroAttrKind::Builtin(s) => Self::Builtin(s.into()),
            hir::def::NonMacroAttrKind::Tool => Self::Tool,
            hir::def::NonMacroAttrKind::DeriveHelper => Self::DeriveHelper,
            hir::def::NonMacroAttrKind::DeriveHelperCompat => Self::DeriveHelperCompat,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::AnonConst> for AnonConst {
    fn from_hir(value: &'hir hir::AnonConst, tcx: TyCtxt<'hir>) -> Self {
        AnonConst {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            body: tcx.hir_body(value.body).hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Body<'hir>> for Body {
    fn from_hir(value: &'hir hir::Body<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Body {
            params: value.params.hir_into(tcx),
            value: value.value.hir_into(tcx),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Param<'hir>> for Param {
    fn from_hir(value: &'hir hir::Param<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Param {
            hir_id: value.hir_id.into(),
            pat: value.pat.hir_into(tcx),
            ty_span: value.ty_span.into(),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Pat<'hir>> for Pat {
    fn from_hir(value: &'hir hir::Pat<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Pat {
            hir_id: value.hir_id.into(),
            kind: Box::new((&value.kind).hir_into(tcx)),
            span: value.span.into(),
            default_binding_modes: value.default_binding_modes,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::PatKind<'hir>> for PatKind {
    fn from_hir(value: &'hir hir::PatKind<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::PatKind::Wild => Self::Wild,
            hir::PatKind::Binding(m, id, ident, p) => Self::Binding {
                mode: m.into(),
                hir_id: id.into(),
                ident: (*ident).into(),
                pat: p.map(|p| p.hir_into(tcx)),
            },
            hir::PatKind::Struct(path, fs, rest) => Self::Struct {
                path: path.hir_into(tcx),
                fields: (*fs).hir_into(tcx),
                rest: *rest,
            },
            hir::PatKind::TupleStruct(path, ps, ddp) => Self::TupleStruct {
                path: path.hir_into(tcx),
                pats: (*ps).hir_into(tcx),
                dot_dot_pos: ddp.into(),
            },
            hir::PatKind::Or(ps) => Self::Or {
                pats: (*ps).hir_into(tcx),
            },
            hir::PatKind::Never => Self::Never,
            hir::PatKind::Tuple(ps, ddp) => Self::Tuple {
                pats: (*ps).hir_into(tcx),
                dot_dot_pos: ddp.into(),
            },
            hir::PatKind::Box(p) => Self::Box {
                pat: (*p).hir_into(tcx),
            },
            hir::PatKind::Deref(p) => Self::Deref {
                pat: (*p).hir_into(tcx),
            },
            hir::PatKind::Ref(p, m) => Self::Ref {
                pat: (*p).hir_into(tcx),
                r#mut: matches!(m, hir::Mutability::Mut),
            },
            hir::PatKind::Range(l, r, i) => Self::Range {
                lhs: l.map(|e| e.hir_into(tcx)),
                rhs: r.map(|e| e.hir_into(tcx)),
                inclusive: matches!(i, hir::RangeEnd::Included),
            },
            hir::PatKind::Slice(ps, p, pss) => Self::Slice {
                start: (*ps).hir_into(tcx),
                mid: p.map(|p| p.hir_into(tcx)),
                rest: (*pss).hir_into(tcx),
            },
            hir::PatKind::Expr(e) => Self::Expr {
                expr: (*e).hir_into(tcx),
            },
            hir::PatKind::Guard(pat, guard) => Self::Guard {
                pat: (*pat).hir_into(tcx),
                guard: (*guard).hir_into(tcx),
            },
            hir::PatKind::Missing => Self::Missing,
            hir::PatKind::Err(_) => Self::Err,
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx hir::PatExpr<'tcx>> for PatExpr {
    fn from_hir(value: &'tcx hir::PatExpr<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            kind: (&value.kind).hir_into(tcx),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx hir::PatExprKind<'tcx>> for PatExprKind {
    fn from_hir(value: &'tcx hir::PatExprKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            rustc_hir::PatExprKind::Lit { lit, negated } => Self::Lit {
                lit: (*lit).into(),
                negated: *negated,
            },
            rustc_hir::PatExprKind::ConstBlock(const_block) => Self::ConstBlock {
                block: const_block.hir_into(tcx),
            },
            rustc_hir::PatExprKind::Path(qpath) => Self::Path {
                path: qpath.hir_into(tcx),
            },
        }
    }
}

impl From<&hir::BindingMode> for BindingMode {
    fn from(value: &hir::BindingMode) -> Self {
        BindingMode {
            by_ref: value.0.into(),
            r#mut: matches!(value.1, hir::Mutability::Mut),
        }
    }
}

impl From<hir::ByRef> for ByRef {
    fn from(value: hir::ByRef) -> Self {
        match value {
            hir::ByRef::Yes(hir::Mutability::Mut) => ByRef::Yes { r#mut: true },
            hir::ByRef::Yes(hir::Mutability::Not) => ByRef::Yes { r#mut: false },
            hir::ByRef::No => ByRef::No,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::PatField<'hir>> for PatField {
    fn from_hir(value: &'hir hir::PatField<'hir>, tcx: TyCtxt<'hir>) -> Self {
        PatField {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            pat: value.pat.hir_into(tcx),
            is_shorthand: value.is_shorthand,
            span: value.span.into(),
        }
    }
}

impl From<&hir::DotDotPos> for DotDotPos {
    fn from(value: &hir::DotDotPos) -> Self {
        DotDotPos(match value.as_opt_usize() {
            Some(n) => n as u32,
            _ => u32::MAX,
        })
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Expr<'hir>> for Expr {
    fn from_hir(value: &'hir hir::Expr<'hir>, tcx: TyCtxt<'hir>) -> Self {
        if let Some(body_id) = get_ghost_expr(tcx, value) {
            convert_ghost_block(tcx, body_id)
        } else {
            let hir_id = value.hir_id.into();
            let mut kind = (&value.kind).hir_into(tcx);
            // Extract possible loop spec
            if let ExprKind::Loop { spec, .. } = &mut kind {
                SPEC_MAP.with_borrow_mut(|r| {
                    let smap = r.as_mut().unwrap();
                    if let Some(ls) = smap.loop_specs.remove(&hir_id) {
                        *spec = Some(ls);
                    }
                });
            }
            Expr {
                hir_id,
                kind: Box::new(kind),
                span: value.span.into(),
            }
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::ExprKind<'hir>> for ExprKind {
    fn from_hir(value: &'hir hir::ExprKind<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::ExprKind::ConstBlock(c) => Self::ConstBlock {
                block: c.hir_into(tcx),
            },
            hir::ExprKind::Array(es) => Self::Array {
                exprs: (*es).hir_into(tcx),
            },
            hir::ExprKind::Call(e, es) => Self::Call {
                callee: (*e).hir_into(tcx),
                args: (*es).hir_into(tcx),
            },
            hir::ExprKind::MethodCall(p, e, es, sp) => Self::MethodCall {
                segment: (*p).hir_into(tcx),
                callee: (*e).hir_into(tcx),
                args: (*es).hir_into(tcx),
                span: (*sp).into(),
            },
            hir::ExprKind::Tup(es) => Self::Tup {
                exprs: (*es).hir_into(tcx),
            },
            hir::ExprKind::Binary(op, l, r) => Self::Binary {
                op: op.into(),
                left: (*l).hir_into(tcx),
                right: (*r).hir_into(tcx),
            },
            hir::ExprKind::Unary(op, e) => Self::Unary {
                op: op.into(),
                expr: (*e).hir_into(tcx),
            },
            hir::ExprKind::Lit(l) => Self::Lit { lit: (*l).into() },
            hir::ExprKind::Cast(e, ty) => Self::Cast {
                expr: (*e).hir_into(tcx),
                ty: (*ty).hir_into(tcx),
            },
            hir::ExprKind::Type(e, ty) => Self::Type {
                expr: (*e).hir_into(tcx),
                ty: (*ty).hir_into(tcx),
            },
            hir::ExprKind::DropTemps(e) => Self::DropTemps {
                expr: (*e).hir_into(tcx),
            },
            hir::ExprKind::Let(l) => Self::Let {
                r#let: (*l).hir_into(tcx),
            },
            hir::ExprKind::If(c, t, e) => Self::If {
                cond: (*c).hir_into(tcx),
                then: (*t).hir_into(tcx),
                els: e.map(|e| e.hir_into(tcx)),
            },
            hir::ExprKind::Loop(b, l, s, sp) => Self::Loop {
                block: (*b).hir_into(tcx),
                label: l.map(Into::into),
                src: s.into(),
                span: (*sp).into(),
                spec: None,
            },
            hir::ExprKind::Match(e, arms, s) => Self::Match {
                expr: (*e).hir_into(tcx),
                arms: (*arms).hir_into(tcx),
                src: s.into(),
            },
            hir::ExprKind::Closure(c) => Self::Closure {
                closure: (*c).hir_into(tcx),
            },
            hir::ExprKind::Block(b, l) => Self::Block {
                block: (*b).hir_into(tcx),
                label: l.map(Into::into),
            },
            hir::ExprKind::Assign(l, r, sp) => Self::Assign {
                left: (*l).hir_into(tcx),
                right: (*r).hir_into(tcx),
                span: (*sp).into(),
            },
            hir::ExprKind::AssignOp(op, l, r) => Self::AssignOp {
                op: op.into(),
                left: (*l).hir_into(tcx),
                right: (*r).hir_into(tcx),
            },
            hir::ExprKind::Field(e, i) => Self::Field {
                expr: (*e).hir_into(tcx),
                field: (*i).into(),
            },
            hir::ExprKind::Index(b, i, sp) => Self::Index {
                base: (*b).hir_into(tcx),
                idx: (*i).hir_into(tcx),
                span: (*sp).into(),
            },
            hir::ExprKind::Path(p) => Self::Path {
                path: p.hir_into(tcx),
            },
            hir::ExprKind::AddrOf(raw, m, e) => Self::AddrOf {
                raw: matches!(raw, hir::BorrowKind::Raw),
                r#mut: matches!(m, hir::Mutability::Mut),
                expr: (*e).hir_into(tcx),
            },
            hir::ExprKind::Break(d, e) => Self::Break {
                dest: d.into(),
                expr: e.map(|e| e.hir_into(tcx)),
            },
            hir::ExprKind::Continue(d) => Self::Continue { dest: d.into() },
            hir::ExprKind::Ret(e) => Self::Ret {
                expr: e.map(|e| e.hir_into(tcx)),
            },
            hir::ExprKind::Become(e) => Self::Become {
                expr: (*e).hir_into(tcx),
            },
            hir::ExprKind::InlineAsm(..) => Self::InlineAsm,
            hir::ExprKind::OffsetOf(ty, is) => Self::OffsetOf {
                ty: (*ty).hir_into(tcx),
                idents: is.iter().copied().map(Into::into).collect(),
            },
            hir::ExprKind::Struct(path, fs, tail) => Self::Struct {
                path: (*path).hir_into(tcx),
                fields: (*fs).hir_into(tcx),
                tail: tail.hir_into(tcx),
            },
            hir::ExprKind::Repeat(e, l) => Self::Repeat {
                expr: (*e).hir_into(tcx),
                len: (*l).hir_into(tcx),
            },
            hir::ExprKind::Yield(e, s) => Self::Yield {
                expr: (*e).hir_into(tcx),
                src: s.into(),
            },
            hir::ExprKind::Use(e, span) => Self::Use {
                expr: (*e).hir_into(tcx),
                span: (*span).into(),
            },
            hir::ExprKind::UnsafeBinderCast(..) => todo!("Unsafe binder cast"),
            hir::ExprKind::Err(_) => Self::Err,
        }
    }
}

impl From<&hir::AssignOp> for AssignOp {
    fn from(value: &hir::AssignOp) -> Self {
        AssignOp {
            span: value.span.into(),
            node: value.node.into(),
        }
    }
}

impl From<hir::AssignOpKind> for AssignOpKind {
    fn from(value: hir::AssignOpKind) -> Self {
        match value {
            rustc_ast::AssignOpKind::AddAssign => Self::AddAssign,
            rustc_ast::AssignOpKind::SubAssign => Self::SubAssign,
            rustc_ast::AssignOpKind::MulAssign => Self::MulAssign,
            rustc_ast::AssignOpKind::DivAssign => Self::DivAssign,
            rustc_ast::AssignOpKind::RemAssign => Self::RemAssign,
            rustc_ast::AssignOpKind::BitXorAssign => Self::BitXorAssign,
            rustc_ast::AssignOpKind::BitAndAssign => Self::BitAndAssign,
            rustc_ast::AssignOpKind::BitOrAssign => Self::BitOrAssign,
            rustc_ast::AssignOpKind::ShlAssign => Self::ShlAssign,
            rustc_ast::AssignOpKind::ShrAssign => Self::ShrAssign,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::StructTailExpr<'hir>> for StructTailExpr {
    fn from_hir(value: &'hir hir::StructTailExpr<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            rustc_hir::StructTailExpr::None => Self::None,
            rustc_hir::StructTailExpr::Base(expr) => Self::Base {
                base: Box::new((*expr).hir_into(tcx)),
            },
            rustc_hir::StructTailExpr::DefaultFields(span) => Self::DefaultFields {
                span: (*span).into(),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::ConstBlock> for ConstBlock {
    fn from_hir(value: &'hir hir::ConstBlock, tcx: TyCtxt<'hir>) -> Self {
        ConstBlock {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            body: tcx.hir_body(value.body).hir_into(tcx),
        }
    }
}

impl From<&hir::UnOp> for UnOp {
    fn from(value: &hir::UnOp) -> Self {
        match value {
            hir::UnOp::Deref => Self::Deref,
            hir::UnOp::Not => Self::Not,
            hir::UnOp::Neg => Self::Neg,
        }
    }
}

impl From<&hir::Lit> for Lit {
    fn from(value: &hir::Lit) -> Self {
        Lit {
            node: (&value.node).into(),
            span: value.span.into(),
        }
    }
}

impl From<&rustc_ast::LitKind> for LitKind {
    fn from(value: &rustc_ast::LitKind) -> Self {
        match value {
            rustc_ast::LitKind::Str(sym, sty) => Self::Str {
                symbol: sym.into(),
                style: sty.into(),
            },
            rustc_ast::LitKind::ByteStr(bytes, sty) => Self::ByteStr {
                bytes: (*bytes).iter().copied().collect(),
                style: sty.into(),
            },
            rustc_ast::LitKind::CStr(bytes, sty) => Self::CStr {
                bytes: (*bytes).iter().copied().collect(),
                style: sty.into(),
            },
            rustc_ast::LitKind::Byte(b) => Self::Byte { byte: *b },
            rustc_ast::LitKind::Char(c) => Self::Char { char: *c },
            rustc_ast::LitKind::Int(v, ty) => Self::Int {
                value: v.0,
                ty: ty.into(),
            },
            rustc_ast::LitKind::Float(sym, ty) => Self::Float {
                symbol: sym.into(),
                ty: ty.into(),
            },
            rustc_ast::LitKind::Bool(b) => Self::Bool { value: *b },
            rustc_ast::LitKind::Err(_) => Self::Err,
        }
    }
}

impl From<&rustc_ast::StrStyle> for StrStyle {
    fn from(value: &rustc_ast::StrStyle) -> Self {
        match value {
            rustc_ast::StrStyle::Cooked => Self::Cooked,
            rustc_ast::StrStyle::Raw(c) => Self::Raw { depth: *c },
        }
    }
}

impl From<&rustc_ast::LitIntType> for LitIntType {
    fn from(value: &rustc_ast::LitIntType) -> Self {
        match value {
            rustc_ast::LitIntType::Signed(ty) => Self::Signed { ty: ty.into() },
            rustc_ast::LitIntType::Unsigned(ty) => Self::Unsigned { ty: ty.into() },
            rustc_ast::LitIntType::Unsuffixed => Self::Unsuffixed,
        }
    }
}

impl From<&rustc_ast::LitFloatType> for LitFloatType {
    fn from(value: &rustc_ast::LitFloatType) -> Self {
        match value {
            rustc_ast::LitFloatType::Suffixed(ty) => Self::Suffixed { ty: ty.into() },
            rustc_ast::LitFloatType::Unsuffixed => Self::Unsuffixed,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::LetExpr<'hir>> for LetExpr {
    fn from_hir(value: &'hir hir::LetExpr<'hir>, tcx: TyCtxt<'hir>) -> Self {
        LetExpr {
            span: value.span.into(),
            pat: value.pat.hir_into(tcx),
            ty: value.ty.map(|ty| ty.hir_into(tcx)),
            init: value.init.hir_into(tcx),
            recovered: matches!(value.recovered, rustc_ast::Recovered::Yes(_)),
        }
    }
}

impl From<&hir::LoopSource> for LoopSource {
    fn from(value: &hir::LoopSource) -> Self {
        match value {
            hir::LoopSource::Loop => Self::Loop,
            hir::LoopSource::While => Self::While,
            hir::LoopSource::ForLoop => Self::ForLoop,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Arm<'hir>> for Arm {
    fn from_hir(value: &'hir hir::Arm<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Arm {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            pat: value.pat.hir_into(tcx),
            guard: value.guard.map(|e| e.hir_into(tcx)),
            body: value.body.hir_into(tcx),
        }
    }
}

impl From<&hir::MatchSource> for MatchSource {
    fn from(value: &hir::MatchSource) -> Self {
        match value {
            hir::MatchSource::Normal => Self::Normal,
            hir::MatchSource::Postfix => Self::Postfix,
            hir::MatchSource::ForLoopDesugar => Self::ForLoopDesugar,
            hir::MatchSource::TryDesugar(id) => Self::TryDesugar { hir_id: id.into() },
            hir::MatchSource::AwaitDesugar => Self::AwaitDesugar,
            hir::MatchSource::FormatArgs => Self::FormatArgs,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Closure<'hir>> for Closure {
    fn from_hir(value: &'hir hir::Closure<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Closure {
            def_id: value.def_id.into(),
            binder: value.binder.into(),
            constness: matches!(value.constness, hir::Constness::Const),
            capture_clause: value.capture_clause.into(),
            bound_generic_params: value.bound_generic_params.hir_into(tcx),
            fn_decl: value.fn_decl.hir_into(tcx),
            body_id: value.body,
            body: tcx.hir_body(value.body).hir_into(tcx),
            fn_decl_span: value.fn_decl_span.into(),
            fn_arg_span: value.fn_arg_span.map(Into::into),
        }
    }
}

impl From<hir::ClosureBinder> for ClosureBinder {
    fn from(value: hir::ClosureBinder) -> Self {
        match value {
            hir::ClosureBinder::For { span } => Self::For { span: span.into() },
            _ => Self::Default,
        }
    }
}

impl From<hir::CaptureBy> for CaptureBy {
    fn from(value: hir::CaptureBy) -> Self {
        match value {
            hir::CaptureBy::Value { move_kw } => Self::Value {
                move_kw: move_kw.into(),
            },
            _ => Self::Ref,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::FnDecl<'hir>> for FnDecl {
    fn from_hir(value: &'hir hir::FnDecl<'hir>, tcx: TyCtxt<'hir>) -> Self {
        FnDecl {
            inputs: value.inputs.hir_into(tcx),
            output: value.output.hir_into(tcx),
            c_variadic: value.c_variadic,
            implicit_self: value.implicit_self.into(),
            lifetime_elision_allowed: value.lifetime_elision_allowed,
        }
    }
}

impl<'hir> FromHir<'hir, hir::FnRetTy<'hir>> for FnRetTy {
    fn from_hir(value: hir::FnRetTy<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::FnRetTy::DefaultReturn(sp) => Self::DefaultReturn { span: sp.into() },
            hir::FnRetTy::Return(ty) => Self::Return {
                ty: ty.hir_into(tcx),
            },
        }
    }
}

impl From<hir::ImplicitSelfKind> for ImplicitSelfKind {
    fn from(value: hir::ImplicitSelfKind) -> Self {
        match value {
            hir::ImplicitSelfKind::Imm => Self::Imm,
            hir::ImplicitSelfKind::Mut => Self::Mut,
            hir::ImplicitSelfKind::RefImm => Self::RefImm,
            hir::ImplicitSelfKind::RefMut => Self::RefMut,
            hir::ImplicitSelfKind::None => Self::None,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Block<'hir>> for Block {
    fn from_hir(value: &'hir hir::Block<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Block {
            stmts: value
                .stmts
                .iter()
                .filter(|s| {
                    // Filter out statements that are actually spec closures.
                    // Of form `let _ = #[attrs] |params| {..};`
                    if let hir::StmtKind::Let(hir::LetStmt {
                        init:
                            Some(hir::Expr {
                                kind: hir::ExprKind::Closure(closure),
                                ..
                            }),
                        ..
                    }) = s.kind
                    {
                        !is_spec(tcx, closure.def_id.to_def_id())
                    } else {
                        true
                    }
                })
                .map(|s| s.hir_into(tcx))
                .collect(),
            expr: value.expr.map(|e| e.hir_into(tcx)),
            hir_id: value.hir_id.into(),
            rules: value.rules.into(),
            span: value.span.into(),
            targeted_by_break: value.targeted_by_break,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::Stmt<'hir>> for Stmt {
    fn from_hir(value: &'hir hir::Stmt<'hir>, tcx: TyCtxt<'hir>) -> Self {
        Stmt {
            hir_id: value.hir_id.into(),
            kind: (&value.kind).hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::StmtKind<'hir>> for StmtKind {
    fn from_hir(value: &'hir hir::StmtKind<'hir>, tcx: TyCtxt<'hir>) -> Self {
        match value {
            hir::StmtKind::Let(l) => Self::Let {
                r#let: (*l).hir_into(tcx),
            },
            hir::StmtKind::Item(i) => Self::Item {
                item: tcx.hir_item(*i).hir_into(tcx),
            },
            hir::StmtKind::Expr(e) => Self::Expr {
                expr: (*e).hir_into(tcx),
            },
            hir::StmtKind::Semi(e) => Self::Semi {
                expr: (*e).hir_into(tcx),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::LetStmt<'hir>> for LetStmt {
    fn from_hir(value: &'hir hir::LetStmt<'hir>, tcx: TyCtxt<'hir>) -> Self {
        LetStmt {
            pat: value.pat.hir_into(tcx),
            ty: value.ty.map(|ty| ty.hir_into(tcx)),
            init: value.init.map(|e| e.hir_into(tcx)),
            els: value.els.map(|b| b.hir_into(tcx)),
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            source: value.source.into(),
        }
    }
}

impl From<hir::LocalSource> for LocalSource {
    fn from(value: hir::LocalSource) -> Self {
        match value {
            hir::LocalSource::Normal => Self::Normal,
            hir::LocalSource::AsyncFn => Self::AsyncFn,
            hir::LocalSource::AwaitDesugar => Self::AwaitDesugar,
            hir::LocalSource::AssignDesugar(sp) => Self::AssignDesugar { span: sp.into() },
            hir::LocalSource::Contract => Self::Contract,
        }
    }
}

impl From<hir::BlockCheckMode> for BlockCheckMode {
    fn from(value: hir::BlockCheckMode) -> Self {
        match value {
            hir::BlockCheckMode::DefaultBlock => Self::DefaultBlock,
            hir::BlockCheckMode::UnsafeBlock(s) => Self::UnsafeBlock { src: s.into() },
        }
    }
}

impl From<hir::UnsafeSource> for UnsafeSource {
    fn from(value: hir::UnsafeSource) -> Self {
        match value {
            hir::UnsafeSource::CompilerGenerated => Self::CompilerGenerated,
            hir::UnsafeSource::UserProvided => Self::UserProvided,
        }
    }
}

impl From<&hir::BinOp> for BinOp {
    fn from(value: &hir::BinOp) -> Self {
        BinOp {
            span: value.span.into(),
            node: value.node.into(),
        }
    }
}

impl From<hir::BinOpKind> for BinOpKind {
    fn from(value: hir::BinOpKind) -> Self {
        match value {
            hir::BinOpKind::Add => Self::Add,
            hir::BinOpKind::Sub => Self::Sub,
            hir::BinOpKind::Mul => Self::Mul,
            hir::BinOpKind::Div => Self::Div,
            hir::BinOpKind::Rem => Self::Rem,
            hir::BinOpKind::And => Self::And,
            hir::BinOpKind::Or => Self::Or,
            hir::BinOpKind::BitXor => Self::BitXor,
            hir::BinOpKind::BitAnd => Self::BitAnd,
            hir::BinOpKind::BitOr => Self::BitOr,
            hir::BinOpKind::Shl => Self::Shl,
            hir::BinOpKind::Shr => Self::Shr,
            hir::BinOpKind::Eq => Self::Eq,
            hir::BinOpKind::Lt => Self::Lt,
            hir::BinOpKind::Le => Self::Le,
            hir::BinOpKind::Ne => Self::Ne,
            hir::BinOpKind::Ge => Self::Ge,
            hir::BinOpKind::Gt => Self::Gt,
        }
    }
}

impl From<&hir::Destination> for Destination {
    fn from(value: &hir::Destination) -> Self {
        Destination {
            label: value.label.map(|l| l.into()),
            target_id: match value.target_id {
                Ok(id) => Ok(id.into()),
                Err(e) => Err(e.into()),
            },
        }
    }
}

impl From<rustc_ast::Label> for Label {
    fn from(value: rustc_ast::Label) -> Self {
        Label {
            ident: value.ident.into(),
        }
    }
}

impl From<hir::LoopIdError> for LoopIdError {
    fn from(value: hir::LoopIdError) -> Self {
        match value {
            hir::LoopIdError::OutsideLoopScope => Self::OutsideLoopScope,
            hir::LoopIdError::UnlabeledCfInWhileCondition => Self::UnlabeledCfInWhileCondition,
            hir::LoopIdError::UnresolvedLabel => Self::UnresolvedLabel,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir hir::ExprField<'hir>> for ExprField {
    fn from_hir(value: &'hir hir::ExprField<'hir>, tcx: TyCtxt<'hir>) -> Self {
        ExprField {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            expr: value.expr.hir_into(tcx),
            span: value.span.into(),
            is_shorthand: value.is_shorthand,
        }
    }
}

impl From<&hir::InferArg> for InferArg {
    fn from(value: &hir::InferArg) -> Self {
        InferArg {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
        }
    }
}

impl From<&hir::YieldSource> for YieldSource {
    fn from(value: &hir::YieldSource) -> Self {
        match value {
            hir::YieldSource::Await { expr } => Self::Await {
                expr: expr.map(|id| id.into()),
            },
            _ => Self::Yield,
        }
    }
}

impl From<hir::GenericParamSource> for GenericParamSource {
    fn from(value: hir::GenericParamSource) -> Self {
        match value {
            hir::GenericParamSource::Generics => Self::Generics,
            hir::GenericParamSource::Binder => Self::Binder,
        }
    }
}

impl<'hir, T, S> FromHir<'hir, &'hir [T]> for Vec<S>
where
    S: FromHir<'hir, &'hir T>,
    T: std::fmt::Debug,
{
    fn from_hir(value: &'hir [T], tcx: TyCtxt<'hir>) -> Self {
        value.iter().map(|i| i.hir_into(tcx)).collect()
    }
}

impl From<&hir::Lifetime> for Lifetime {
    fn from(value: &hir::Lifetime) -> Self {
        Lifetime {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            kind: value.kind.into(),
            source: value.source.into(),
            syntax: value.syntax.into(),
        }
    }
}

impl From<hir::LifetimeKind> for LifetimeKind {
    fn from(value: hir::LifetimeKind) -> Self {
        match value {
            hir::LifetimeKind::Param(ldid) => Self::Param { id: ldid.into() },
            hir::LifetimeKind::ImplicitObjectLifetimeDefault => Self::ImplicitObjectLifetimeDefault,
            hir::LifetimeKind::Error => Self::Error,
            hir::LifetimeKind::Infer => Self::Infer,
            hir::LifetimeKind::Static => Self::Static,
        }
    }
}

impl From<hir::LifetimeSource> for LifetimeSource {
    fn from(value: hir::LifetimeSource) -> Self {
        match value {
            rustc_hir::LifetimeSource::Reference => Self::Reference,
            rustc_hir::LifetimeSource::Path { angle_brackets } => Self::Path {
                angle_brackets: angle_brackets.into(),
            },
            rustc_hir::LifetimeSource::OutlivesBound => Self::OutlivesBound,
            rustc_hir::LifetimeSource::PreciseCapturing => Self::PreciseCapturing,
            rustc_hir::LifetimeSource::Other => Self::Other,
        }
    }
}

impl From<hir::AngleBrackets> for AngleBrackets {
    fn from(value: hir::AngleBrackets) -> Self {
        match value {
            rustc_hir::AngleBrackets::Missing => Self::Missing,
            rustc_hir::AngleBrackets::Empty => Self::Empty,
            rustc_hir::AngleBrackets::Full => Self::Full,
        }
    }
}

impl From<hir::LifetimeSyntax> for LifetimeSyntax {
    fn from(value: hir::LifetimeSyntax) -> Self {
        match value {
            rustc_hir::LifetimeSyntax::Hidden => Self::Hidden,
            rustc_hir::LifetimeSyntax::Anonymous => Self::Anonymous,
            rustc_hir::LifetimeSyntax::Named => Self::Named,
        }
    }
}

impl From<hir::GenericArgsParentheses> for GenericArgsParentheses {
    fn from(value: hir::GenericArgsParentheses) -> Self {
        match value {
            hir::GenericArgsParentheses::No => GenericArgsParentheses::No,
            hir::GenericArgsParentheses::ReturnTypeNotation => {
                GenericArgsParentheses::ReturnTypeNotation
            }
            hir::GenericArgsParentheses::ParenSugar => GenericArgsParentheses::ParenSugar,
        }
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::Ty<'tcx>> for Ty {
    fn from_hir(value: &rustc_middle::ty::Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value.kind() {
            rustc_type_ir::TyKind::Bool => Self::Bool,
            rustc_type_ir::TyKind::Char => Self::Char,
            rustc_type_ir::TyKind::Int(int_ty) => Self::Int { ty: int_ty.into() },
            rustc_type_ir::TyKind::Uint(uint_ty) => Self::Uint { ty: uint_ty.into() },
            rustc_type_ir::TyKind::Float(float_ty) => Self::Float {
                ty: float_ty.into(),
            },
            rustc_type_ir::TyKind::Adt(def, args) => Self::Adt {
                def: def.into(),
                args: args.iter().map(|a| (&a).hir_into(tcx)).collect(),
            },
            rustc_type_ir::TyKind::Foreign(did) => Self::Foreign { def_id: did.into() },
            rustc_type_ir::TyKind::Str => Self::Str,
            rustc_type_ir::TyKind::Array(ty, len) => Self::Array {
                ty: Box::new(ty.hir_into(tcx)),
                len: Box::new(len.hir_into(tcx)),
            },
            rustc_type_ir::TyKind::Pat(ty, pat) => Self::Pat {
                ty: Box::new(ty.hir_into(tcx)),
                pat: Box::new(pat.hir_into(tcx)),
            },
            rustc_type_ir::TyKind::Slice(ty) => Self::Slice {
                ty: Box::new(ty.hir_into(tcx)),
            },
            rustc_type_ir::TyKind::RawPtr(ty, mutability) => Self::RawPtr {
                ty: Box::new(ty.hir_into(tcx)),
                r#mut: mut_to_bool(mutability),
            },
            rustc_type_ir::TyKind::Ref(_, ty, mutability) => Self::Ref {
                ty: Box::new(ty.hir_into(tcx)),
                r#mut: mut_to_bool(mutability),
            },
            rustc_type_ir::TyKind::FnDef(did, args) => Self::FnDef {
                def_id: did.into(),
                args: args.iter().map(|a| a.unpack().hir_into(tcx)).collect(),
            },
            rustc_type_ir::TyKind::FnPtr(binder, fn_header) => Self::FnPtr {
                binder: binder.hir_into(tcx),
                header: fn_header.into(),
            },
            rustc_type_ir::TyKind::Dynamic(binders, _, dyn_kind) => Self::Dynamic {
                binders: binders.into_iter().map(|b| (&b).hir_into(tcx)).collect(),
                kind: dyn_kind.into(),
            },
            rustc_type_ir::TyKind::Closure(did, args) => Self::Closure {
                def_id: did.into(),
                args: args.into_iter().map(|a| (&a).hir_into(tcx)).collect(),
            },
            rustc_type_ir::TyKind::CoroutineClosure(..) => Self::CoroutineClosure {},
            rustc_type_ir::TyKind::Coroutine(..) => Self::Coroutine {},
            rustc_type_ir::TyKind::CoroutineWitness(..) => Self::CoroutineWitness {},
            rustc_type_ir::TyKind::Never => Self::Never,
            rustc_type_ir::TyKind::Tuple(tys) => Self::Tuple {
                tys: tys.iter().map(|ty| (&ty).hir_into(tcx)).collect(),
            },
            rustc_type_ir::TyKind::Alias(alias_ty_kind, alias_ty) => Self::Alias {
                kind: alias_ty_kind.into(),
                ty: alias_ty.hir_into(tcx),
            },
            rustc_type_ir::TyKind::Param(p) => Self::Param { ty: p.into() },
            rustc_type_ir::TyKind::Bound(debruijn_index, ty) => Self::Bound {
                idx: debruijn_index.into(),
                ty: ty.into(),
            },
            rustc_type_ir::TyKind::Placeholder(p) => Self::Placeholder {
                placeholder: p.hir_into(tcx),
            },
            rustc_type_ir::TyKind::Infer(_) => {
                // panic!("Infer types should probably not be encountered at this point")
                Self::Infer {}
            }
            rustc_type_ir::TyKind::UnsafeBinder(..) => todo!("Ty::UnsafeBinder"),
            rustc_type_ir::TyKind::Error(_) => Self::Error,
        }
    }
}

impl<'tcx> From<&rustc_middle::ty::AdtDef<'tcx>> for AdtDef {
    fn from(value: &rustc_middle::ty::AdtDef<'tcx>) -> Self {
        let og_variants = value.variants();
        let mut variants = HashMap::with_capacity(og_variants.len());
        for idx in og_variants.indices() {
            let def = &og_variants[idx];
            variants.insert(VariantIdx(idx.as_u32()), def.into());
        }
        Self {
            did: (&value.did()).into(),
            variants,
            flags: AdtFlags(value.flags().bits()),
        }
    }
}

impl From<&rustc_middle::ty::VariantDef> for VariantDef {
    fn from(value: &rustc_middle::ty::VariantDef) -> Self {
        let og_fields = &value.fields;
        let mut fields = HashMap::with_capacity(og_fields.len());
        for idx in og_fields.indices() {
            let def = &og_fields[idx];
            fields.insert(FieldIdx(idx.as_u32()), def.into());
        }
        Self {
            def_id: (&value.def_id).into(),
            ctor: value
                .ctor
                .as_ref()
                .map(|(kind, did)| (kind.into(), did.into())),
            name: value.name.into(),
            discr: value.discr.into(),
            fields,
        }
    }
}

impl From<&rustc_middle::ty::FieldDef> for TyFieldDef {
    fn from(value: &rustc_middle::ty::FieldDef) -> Self {
        Self {
            did: (&value.did).into(),
            name: value.name.into(),
        }
    }
}

impl From<&rustc_hir::def::CtorKind> for CtorKind {
    fn from(value: &rustc_hir::def::CtorKind) -> Self {
        match value {
            rustc_hir::def::CtorKind::Fn => Self::Fn,
            rustc_hir::def::CtorKind::Const => Self::Const,
        }
    }
}

impl From<rustc_middle::ty::VariantDiscr> for VariantDiscr {
    fn from(value: rustc_middle::ty::VariantDiscr) -> Self {
        match value {
            rustc_middle::ty::VariantDiscr::Explicit(def_id) => Self::Explicit {
                def_id: (&def_id).into(),
            },
            rustc_middle::ty::VariantDiscr::Relative(i) => Self::Relative { idx: i },
        }
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::Pattern<'tcx>> for Pattern {
    fn from_hir(value: &rustc_middle::ty::Pattern<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value.0.0 {
            rustc_middle::ty::PatternKind::Range { start, end } => Self::Range {
                start: start.hir_into(tcx),
                end: end.hir_into(tcx),
            },
            rustc_middle::ty::PatternKind::Or(pats) => Self::Or {
                pats: pats.iter().map(|p| (&p).hir_into(tcx)).collect(),
            },
        }
    }
}

impl<'tcx> FromHir<'tcx, rustc_middle::ty::FnSigTys<TyCtxt<'tcx>>> for FnSigTys {
    fn from_hir(value: rustc_middle::ty::FnSigTys<TyCtxt<'tcx>>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            inputs_and_output: value
                .inputs_and_output
                .iter()
                .map(|t| (&t).hir_into(tcx))
                .collect(),
        }
    }
}

impl<'tcx> From<&rustc_type_ir::FnHeader<TyCtxt<'tcx>>> for FnHeader {
    fn from(value: &rustc_type_ir::FnHeader<TyCtxt<'tcx>>) -> Self {
        Self {
            safety: matches!(value.safety, rustc_hir::Safety::Safe),
            constness: false,
            asyncness: false,
        }
    }
}

impl<'tcx> FromHir<'tcx, rustc_middle::ty::ExistentialPredicate<'tcx>> for ExistentialPredicate {
    fn from_hir(value: rustc_middle::ty::ExistentialPredicate<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            rustc_type_ir::ExistentialPredicate::Trait(existential_trait_ref) => Self::Trait {
                pred: existential_trait_ref.hir_into(tcx),
            },
            rustc_type_ir::ExistentialPredicate::Projection(existential_projection) => {
                Self::Projection {
                    pred: existential_projection.hir_into(tcx),
                }
            }
            rustc_type_ir::ExistentialPredicate::AutoTrait(did) => Self::AutoTrait {
                def_id: (&did).into(),
            },
        }
    }
}

impl<'tcx> FromHir<'tcx, rustc_middle::ty::ExistentialTraitRef<'tcx>> for ExistentialTraitRef {
    fn from_hir(value: rustc_middle::ty::ExistentialTraitRef<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            def_id: (&value.def_id).into(),
            args: value.args.iter().map(|a| (&a).hir_into(tcx)).collect(),
        }
    }
}

impl<'tcx> FromHir<'tcx, rustc_middle::ty::ExistentialProjection<'tcx>> for ExistentialProjection {
    fn from_hir(value: rustc_middle::ty::ExistentialProjection<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            def_id: (&value.def_id).into(),
            args: value.args.iter().map(|a| (&a).hir_into(tcx)).collect(),
            term: (&value.term).hir_into(tcx),
        }
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::Term<'tcx>> for TyTerm {
    fn from_hir(value: &rustc_middle::ty::Term<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match &value.unpack() {
            rustc_type_ir::TermKind::Ty(ty) => Self::Ty {
                ty: ty.hir_into(tcx),
            },
            rustc_type_ir::TermKind::Const(c) => Self::Const { c: c.hir_into(tcx) },
        }
    }
}

impl<'tcx, S, T> FromHir<'tcx, &rustc_middle::ty::Binder<'tcx, T>> for Binder<S>
where
    T: HirInto<'tcx, S> + Clone,
{
    fn from_hir(value: &rustc_middle::ty::Binder<'tcx, T>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            value: value.clone().skip_binder().hir_into(tcx),
            bound_vars: value.bound_vars().iter().map(|v| v.into()).collect(),
        }
    }
}

impl From<rustc_middle::ty::BoundVariableKind> for BoundVarKind {
    fn from(value: rustc_middle::ty::BoundVariableKind) -> Self {
        match value {
            rustc_middle::ty::BoundVariableKind::Ty(bound_ty_kind) => Self::Ty {
                kind: bound_ty_kind.into(),
            },
            rustc_middle::ty::BoundVariableKind::Region(bound_region_kind) => Self::Region {
                region: bound_region_kind.into(),
            },
            rustc_middle::ty::BoundVariableKind::Const => Self::Const,
        }
    }
}

impl From<rustc_middle::ty::BoundRegionKind> for BoundRegionKind {
    fn from(value: rustc_middle::ty::BoundRegionKind) -> Self {
        match value {
            rustc_middle::ty::BoundRegionKind::Anon => Self::Anon,
            rustc_middle::ty::BoundRegionKind::Named(def_id, symbol) => Self::Named {
                def_id: (&def_id).into(),
                symbol: symbol.into(),
            },
            rustc_middle::ty::BoundRegionKind::ClosureEnv => Self::ClosureEnv,
        }
    }
}

impl From<&rustc_middle::ty::DynKind> for DynKind {
    fn from(value: &rustc_middle::ty::DynKind) -> Self {
        match value {
            rustc_type_ir::DynKind::Dyn => Self::Dyn,
            rustc_type_ir::DynKind::DynStar => Self::DynStar,
        }
    }
}

impl From<&rustc_middle::ty::ParamTy> for ParamTy {
    fn from(value: &rustc_middle::ty::ParamTy) -> Self {
        Self {
            index: value.index,
            name: value.name.into(),
        }
    }
}

impl From<&rustc_type_ir::DebruijnIndex> for DebruijnIndex {
    fn from(value: &rustc_type_ir::DebruijnIndex) -> Self {
        Self(value.as_u32())
    }
}

impl From<&rustc_middle::ty::BoundTy> for BoundTy {
    fn from(value: &rustc_middle::ty::BoundTy) -> Self {
        Self {
            var: value.var.into(),
            kind: value.kind.into(),
        }
    }
}

impl From<rustc_middle::ty::BoundTyKind> for BoundTyKind {
    fn from(value: rustc_middle::ty::BoundTyKind) -> Self {
        match value {
            rustc_middle::ty::BoundTyKind::Anon => Self::Anon,
            rustc_middle::ty::BoundTyKind::Param(def_id, symbol) => Self::Param {
                def_id: (&def_id).into(),
                symbol: symbol.into(),
            },
        }
    }
}

#[inline]
fn mut_to_bool(m: &rustc_ast_ir::Mutability) -> bool {
    match m {
        rustc_ast::Mutability::Not => false,
        rustc_ast::Mutability::Mut => true,
    }
}

impl From<&rustc_middle::ty::IntTy> for IntTy {
    fn from(value: &rustc_middle::ty::IntTy) -> Self {
        match value {
            rustc_type_ir::IntTy::Isize => Self::Isize,
            rustc_type_ir::IntTy::I8 => Self::I8,
            rustc_type_ir::IntTy::I16 => Self::I16,
            rustc_type_ir::IntTy::I32 => Self::I32,
            rustc_type_ir::IntTy::I64 => Self::I64,
            rustc_type_ir::IntTy::I128 => Self::I128,
        }
    }
}

impl From<&rustc_middle::ty::UintTy> for UintTy {
    fn from(value: &rustc_middle::ty::UintTy) -> Self {
        match value {
            rustc_type_ir::UintTy::Usize => Self::Usize,
            rustc_type_ir::UintTy::U8 => Self::U8,
            rustc_type_ir::UintTy::U16 => Self::U16,
            rustc_type_ir::UintTy::U32 => Self::U32,
            rustc_type_ir::UintTy::U64 => Self::U64,
            rustc_type_ir::UintTy::U128 => Self::U128,
        }
    }
}

impl From<&rustc_middle::ty::FloatTy> for FloatTy {
    fn from(value: &rustc_middle::ty::FloatTy) -> Self {
        match value {
            rustc_type_ir::FloatTy::F16 => Self::F16,
            rustc_type_ir::FloatTy::F32 => Self::F32,
            rustc_type_ir::FloatTy::F64 => Self::F64,
            rustc_type_ir::FloatTy::F128 => Self::F128,
        }
    }
}

impl From<&rustc_middle::ty::AliasTyKind> for AliasTyKind {
    fn from(value: &rustc_middle::ty::AliasTyKind) -> Self {
        match value {
            rustc_type_ir::AliasTyKind::Projection => Self::Projection,
            rustc_type_ir::AliasTyKind::Inherent => Self::Inherent,
            rustc_type_ir::AliasTyKind::Opaque => Self::Opaque,
            rustc_type_ir::AliasTyKind::Free => Self::Free,
        }
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::AliasTy<'tcx>> for AliasTy {
    fn from_hir(value: &rustc_middle::ty::AliasTy<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            args: value
                .args
                .iter()
                .map(|i| i.unpack().hir_into(tcx))
                .collect(),
            def_id: (&value.def_id).into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, rustc_middle::ty::GenericArgKind<'tcx>> for GenericTyArgKind {
    fn from_hir(value: rustc_middle::ty::GenericArgKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            rustc_type_ir::GenericArgKind::Lifetime(_) => Self::Lifetime,
            rustc_type_ir::GenericArgKind::Type(ty) => Self::Type {
                ty: (&ty).hir_into(tcx),
            },
            rustc_type_ir::GenericArgKind::Const(c) => Self::Const {
                r#const: (&c).hir_into(tcx),
            },
        }
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::Const<'tcx>> for Const {
    fn from_hir(value: &rustc_middle::ty::Const<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value.kind() {
            rustc_type_ir::ConstKind::Param(p) => Self::Param { pc: p.into() },
            rustc_type_ir::ConstKind::Infer(_) => Self::Infer,
            rustc_type_ir::ConstKind::Bound(debruijn_index, bv) => Self::Bound {
                idx: (&debruijn_index).into(),
                bound_var: bv.into(),
            },
            rustc_type_ir::ConstKind::Placeholder(_) => Self::Placeholder,
            rustc_type_ir::ConstKind::Unevaluated(unevaluated_const) => Self::Unevaluated {
                uc: unevaluated_const.hir_into(tcx),
            },
            rustc_type_ir::ConstKind::Value(value) => Self::Value {
                value: value.hir_into(tcx),
            },
            rustc_type_ir::ConstKind::Error(_) => Self::Error,
            rustc_type_ir::ConstKind::Expr(e) => Self::Expr {
                expr: (&e).hir_into(tcx),
            },
        }
    }
}

impl<'tcx> FromHir<'tcx, rustc_middle::ty::Value<'tcx>> for Value {
    fn from_hir(value: rustc_middle::ty::Value<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            ty: (&value.ty).hir_into(tcx),
            valtree: (&value.valtree).into(),
        }
    }
}

impl From<rustc_middle::ty::ParamConst> for ParamConst {
    fn from(value: rustc_middle::ty::ParamConst) -> Self {
        Self {
            index: value.index,
            name: value.name.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, rustc_type_ir::UnevaluatedConst<TyCtxt<'tcx>>> for UnevaluatedConst {
    fn from_hir(value: rustc_type_ir::UnevaluatedConst<TyCtxt<'tcx>>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            def: (&value.def).into(),
            args: value.args.deref().hir_into(tcx),
        }
    }
}

impl From<&rustc_middle::ty::ValTree<'_>> for ValTree {
    fn from(value: &rustc_middle::ty::ValTree<'_>) -> Self {
        let kind: &rustc_middle::ty::ValTreeKind<'_> = value.deref();
        match kind {
            rustc_middle::ty::ValTreeKind::Leaf(scalar_int) => Self::Leaf {
                scalar_int: scalar_int.into(),
            },
            rustc_middle::ty::ValTreeKind::Branch(trees) => Self::Branch {
                branches: trees.into_iter().map(Into::into).collect(),
            },
        }
    }
}

impl From<&rustc_middle::ty::ScalarInt> for ScalarInt {
    fn from(value: &rustc_middle::ty::ScalarInt) -> Self {
        Self {
            data: value.to_bits(value.size()),
            size: NonZero::new(value.size().bits() as u8).unwrap(),
        }
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::Expr<'tcx>> for ConstExpr {
    fn from_hir(value: &rustc_middle::ty::Expr<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            kind: value.kind.into(),
            args: value.args().deref().hir_into(tcx),
        }
    }
}

impl From<rustc_middle::ty::ExprKind> for ConstExprKind {
    fn from(value: rustc_middle::ty::ExprKind) -> Self {
        match value {
            rustc_middle::ty::ExprKind::Binop(bin_op) => Self::Binop {
                kind: bin_op.into(),
            },
            rustc_middle::ty::ExprKind::UnOp(un_op) => Self::UnOp { op: un_op.into() },
            rustc_middle::ty::ExprKind::FunctionCall => Self::FunctionCall,
            rustc_middle::ty::ExprKind::Cast(cast_kind) => Self::Cast {
                kind: cast_kind.into(),
            },
        }
    }
}

impl From<rustc_middle::mir::BinOp> for BinOpKind {
    fn from(value: rustc_middle::mir::BinOp) -> Self {
        match value {
            rustc_middle::mir::BinOp::Add => Self::Add,
            rustc_middle::mir::BinOp::AddUnchecked => Self::Add,
            rustc_middle::mir::BinOp::AddWithOverflow => Self::Add,
            rustc_middle::mir::BinOp::Sub => Self::Sub,
            rustc_middle::mir::BinOp::SubUnchecked => Self::Sub,
            rustc_middle::mir::BinOp::SubWithOverflow => Self::Sub,
            rustc_middle::mir::BinOp::Mul => Self::Mul,
            rustc_middle::mir::BinOp::MulUnchecked => Self::Mul,
            rustc_middle::mir::BinOp::MulWithOverflow => Self::Mul,
            rustc_middle::mir::BinOp::Div => Self::Div,
            rustc_middle::mir::BinOp::Rem => Self::Rem,
            rustc_middle::mir::BinOp::BitXor => Self::BitXor,
            rustc_middle::mir::BinOp::BitAnd => Self::BitAnd,
            rustc_middle::mir::BinOp::BitOr => Self::BitOr,
            rustc_middle::mir::BinOp::Shl => Self::Shl,
            rustc_middle::mir::BinOp::ShlUnchecked => Self::Shl,
            rustc_middle::mir::BinOp::Shr => Self::Shr,
            rustc_middle::mir::BinOp::ShrUnchecked => Self::Shr,
            rustc_middle::mir::BinOp::Eq => Self::Eq,
            rustc_middle::mir::BinOp::Lt => Self::Lt,
            rustc_middle::mir::BinOp::Le => Self::Le,
            rustc_middle::mir::BinOp::Ne => Self::Ne,
            rustc_middle::mir::BinOp::Ge => Self::Ge,
            rustc_middle::mir::BinOp::Gt => Self::Gt,
            rustc_middle::mir::BinOp::Cmp => Self::Cmp,
            rustc_middle::mir::BinOp::Offset => Self::Offset,
        }
    }
}

impl From<rustc_middle::mir::UnOp> for UnOp {
    fn from(value: rustc_middle::mir::UnOp) -> Self {
        match value {
            rustc_middle::mir::UnOp::Not => Self::Not,
            rustc_middle::mir::UnOp::Neg => Self::Neg,
            rustc_middle::mir::UnOp::PtrMetadata => Self::PtrMetadata,
        }
    }
}

impl From<rustc_middle::ty::abstract_const::CastKind> for CastKind {
    fn from(value: rustc_middle::ty::abstract_const::CastKind) -> Self {
        match value {
            rustc_middle::ty::abstract_const::CastKind::As => Self::As,
            rustc_middle::ty::abstract_const::CastKind::Use => Self::Use,
        }
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::GenericArg<'tcx>> for GenericTyArgKind {
    fn from_hir(value: &rustc_middle::ty::GenericArg<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value.unpack() {
            rustc_type_ir::GenericArgKind::Lifetime(_) => Self::Lifetime,
            rustc_type_ir::GenericArgKind::Type(ty) => Self::Type {
                ty: (&ty).hir_into(tcx),
            },
            rustc_type_ir::GenericArgKind::Const(c) => Self::Const {
                r#const: (&c).hir_into(tcx),
            },
        }
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::Placeholder<rustc_middle::ty::BoundTy>>
    for Placeholder<BoundTy>
{
    fn from_hir(
        value: &rustc_middle::ty::Placeholder<rustc_middle::ty::BoundTy>,
        tcx: TyCtxt<'tcx>,
    ) -> Self {
        Placeholder {
            universe: value.universe.into(),
            bound: (&value.bound).hir_into(tcx),
        }
    }
}

impl From<rustc_middle::ty::UniverseIndex> for UniverseIndex {
    fn from(value: rustc_middle::ty::UniverseIndex) -> Self {
        UniverseIndex(value.as_u32())
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::BoundTy> for BoundTy {
    fn from_hir(value: &rustc_middle::ty::BoundTy, tcx: TyCtxt<'tcx>) -> Self {
        BoundTy {
            var: value.var.into(),
            kind: (&value.kind).hir_into(tcx),
        }
    }
}

impl From<rustc_middle::ty::BoundVar> for BoundVar {
    fn from(value: rustc_middle::ty::BoundVar) -> Self {
        Self(value.as_u32())
    }
}

impl<'tcx> FromHir<'tcx, &rustc_middle::ty::BoundTyKind> for BoundTyKind {
    fn from_hir(value: &rustc_middle::ty::BoundTyKind, _: TyCtxt<'tcx>) -> Self {
        match value {
            rustc_middle::ty::BoundTyKind::Anon => Self::Anon,
            rustc_middle::ty::BoundTyKind::Param(def_id, symbol) => Self::Param {
                def_id: def_id.into(),
                symbol: symbol.into(),
            },
        }
    }
}
