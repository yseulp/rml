use super::*;
use crate::spec::SpecCase;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Item {
    pub owner_id: OwnerId,
    pub kind: ItemKind,
    pub span: Span,
    pub vis_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum ItemKind {
    ExternCrate {
        symbol: Option<Symbol>,
        ident: Ident,
    },
    Use {
        path: UsePath,
        use_kind: UseKind,
    },
    Static {
        ident: Ident,
        ty: HirTy,
        r#const: bool,
        body: Body,
    },
    Const {
        ty: HirTy,
        ident: Ident,
        generics: Generics,
        #[serde(skip)]
        body_id: rustc_hir::BodyId,
        body: Body,
    },
    Fn {
        ident: Ident,
        sig: FnSig,
        generics: Generics,
        #[serde(skip)]
        body_id: rustc_hir::BodyId,
        body: Body,
        /// All cases of the specification.
        spec_cases: Vec<SpecCase>,
        ty_generics: Option<TyGenerics>,
    },
    // Macro(MacroDef, MacroKind),
    Mod {
        ident: Ident,
        r#mod: Mod,
    },
    // ForeignMod {
    // abi: Abi,
    // items: [ForeignItemRef],
    // },
    // GlobalAsm(InlineAsm),
    TyAlias {
        ident: Ident,
        ty: HirTy,
        generics: Generics,
    },
    // OpaqueTy(OpaqueTy),
    Enum {
        ident: Ident,
        def: EnumDef,
        generics: Generics,
    },
    Struct {
        ident: Ident,
        data: VariantData,
        generics: Generics,
    },
    Union {
        ident: Ident,
        data: VariantData,
        generics: Generics,
    },
    Trait {
        is_auto: bool,
        is_safe: bool,
        ident: Ident,
        generics: Generics,
        bounds: GenericBounds,
        refs: Vec<TraitItemRef>,
    },
    TraitAlias {
        ident: Ident,
        generics: Generics,
        bounds: GenericBounds,
    },
    Impl {
        r#impl: Impl,
    },
    Macro {
        ident: Ident,
        def: MacroDef,
        kind: MacroKind,
    },
    ForeignMod,
    GlobalAsm,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MacroDef {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Body {
    pub params: Vec<Param>,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Param {
    pub hir_id: HirId,
    pub pat: Pat,
    pub ty_span: Span,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FnSig {
    pub header: FnHeader,
    pub decl: FnDecl,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FnHeader {
    pub safety: bool,
    pub constness: bool,
    pub asyncness: bool,
    // pub abi: Abi,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Mod {
    pub spans: ModSpans,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ModSpans {
    pub inner_span: Span,
    pub inject_use_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EnumDef {
    pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Variant {
    pub ident: Ident,
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    pub data: VariantData,
    pub disr_expr: Option<AnonConst>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum VariantData {
    Struct {
        fields: Vec<FieldDef>,
        recovered: bool,
    },
    Tuple {
        def: Vec<FieldDef>,
        hir_id: HirId,
        local_def_id: LocalDefId,
    },
    Unit {
        hir_id: HirId,
        local_def_id: LocalDefId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FieldDef {
    pub span: Span,
    pub vis_span: Span,
    pub ident: Ident,
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    pub ty: HirTy,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TraitItemRef {
    pub id: TraitItemId,
    pub ident: Ident,
    pub kind: AssocItemKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TraitItemId {
    pub owner_id: OwnerId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Impl {
    pub constness: bool,
    pub safety: bool,
    pub polarity: ImplPolarity,
    pub defaultness: Defaultness,
    pub defaultness_span: Option<Span>,
    pub generics: Generics,
    pub of_trait: Option<TraitRef>,
    pub self_ty: HirTy,
    pub items: Vec<ImplItemRef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum ImplPolarity {
    Positive,
    Negative { span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ImplItemRef {
    pub id: ImplItemId,
    pub ident: Ident,
    pub kind: AssocItemKind,
    pub span: Span,
    pub trait_item_def_id: Option<DefId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum HirTyKind {
    InferDelegation {
        def_id: DefId,
        kind: InferDelegationKind,
    },
    Slice {
        ty: HirTy,
    },
    Array {
        ty: HirTy,
        len: ConstArg,
    },
    Ptr {
        ty: MutHirTy,
    },
    Ref {
        lifetime: Lifetime,
        ty: MutHirTy,
    },
    BareFn {
        ty: BareFnHirTy,
    },
    Never,
    Tup {
        tys: Vec<HirTy>,
    },
    AnonAdt {
        item: Item,
    },
    Path {
        path: QPath,
    },
    // OpaqueDef(Item, Vec<GenericArg>, bool),
    TraitObject {
        refs: Vec<PolyTraitRef>,
        lifetime: Lifetime,
        syntax: TraitObjectSyntax,
    },
    Typeof {
        r#const: AnonConst,
    },
    Infer,
    Err,
    Pat {
        ty: HirTy,
        pat: TyPat,
    },
    OpaqueDef,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TyPat {
    pub hir_id: HirId,
    pub kind: TyPatKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum TyPatKind {
    Range { start: ConstArg, end: ConstArg },
    Or { pats: Vec<TyPat> },
    Err,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum AssocItemKind {
    Const,
    Fn { has_self: bool },
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum TraitObjectSyntax {
    Dyn,
    DynStar,
    None,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum InferDelegationKind {
    Input { id: usize },
    Output,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MutHirTy {
    pub ty: HirTy,
    pub mutbl: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BareFnHirTy {
    pub safety: bool,
    // pub abi: Abi,
    pub generic_params: Vec<GenericParam>,
    pub decl: FnDecl,
    pub param_idents: Vec<Option<Ident>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FnDecl {
    pub inputs: Vec<HirTy>,
    pub output: FnRetTy,
    pub c_variadic: bool,
    pub implicit_self: ImplicitSelfKind,
    pub lifetime_elision_allowed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum FnRetTy {
    DefaultReturn { span: Span },
    Return { ty: HirTy },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ImplicitSelfKind {
    Imm,
    Mut,
    RefImm,
    RefMut,
    None,
}

pub type UsePath = Path<Vec<Res>>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum UseKind {
    Single { ident: Ident },
    Glob,
    ListStem,
}
