use expr::*;
use item::*;
use pat::*;
use serde::Serialize;
use stmt::*;
use ty::*;

pub mod conversion;
pub mod expr;
pub mod item;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod type_extract;
pub mod visit;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Crate {
    pub top_mod: Mod,
    pub types: Vec<HirIdTypeMapping>,
    pub adts: Vec<DefIdAdtMapping>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HirIdTypeMapping {
    pub hir_id: HirId,
    pub ty: Ty,
}

impl From<(HirId, Ty)> for HirIdTypeMapping {
    fn from(value: (HirId, Ty)) -> Self {
        Self {
            hir_id: value.0,
            ty: value.1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DefIdAdtMapping {
    pub def_id: DefId,
    pub def: AdtDef,
}

impl From<(DefId, AdtDef)> for DefIdAdtMapping {
    fn from(value: (DefId, AdtDef)) -> Self {
        Self {
            def_id: value.0,
            def: value.1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Symbol(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct BytePos(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SyntaxContext(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct LocalDefId {
    pub local_def_index: DefIndex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct DefIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
    // pub ctxt: SyntaxContext,
    pub parent: Option<LocalDefId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct OwnerId {
    pub def_id: LocalDefId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HirTy {
    pub hir_id: HirId,
    pub kind: Box<HirTyKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub predicates: Vec<WherePredicate>,
    pub has_where_clause_predicates: bool,
    pub where_clause_span: Span,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct WherePredicate {
    pub hir_id: HirId,
    pub span: Span,
    pub kind: WherePredicateKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum WherePredicateKind {
    Bound(WhereBoundPredicate),
    Region(WhereRegionPredicate),
    Eq(WhereEqPredicate),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct WhereBoundPredicate {
    pub origin: PredicateOrigin,
    pub bound_generic_params: Vec<GenericParam>,
    pub bounded_ty: HirTy,
    pub bounds: GenericBounds,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum PredicateOrigin {
    WhereClause,
    GenericParam,
    ImplTrait,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct WhereRegionPredicate {
    pub in_where_clause: bool,
    pub lifetime: Lifetime,
    pub bounds: GenericBounds,
}

pub type GenericBounds = Vec<GenericBound>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct WhereEqPredicate {
    pub lhs_ty: HirTy,
    pub rhs_ty: HirTy,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum Defaultness {
    Default { has_value: bool },
    Final,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ImplItemId {
    pub owner_id: OwnerId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ItemId {
    pub owner_id: OwnerId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Path<R = Res> {
    pub span: Span,
    pub res: R,
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Def {
    pub kind: DefKind,
    pub id: DefId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum Res<Id = HirId> {
    Def {
        def: Def,
    },
    PrimTy {
        ty: PrimHirTy,
    },
    SelfTyParam {
        trait_: DefId,
    },
    SelfTyAlias {
        alias_to: DefId,
        forbid_generic: bool,
        is_trait_impl: bool,
    },
    SelfCtor(DefId),
    Local {
        id: Id,
    },
    ToolMod,
    NonMacroAttr(NonMacroAttrKind),
    Err,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct HirId {
    pub owner: OwnerId,
    pub local_id: ItemLocalId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct ItemLocalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct DefId {
    pub index: DefIndex,
    pub krate: CrateNum,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct CrateNum(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
/// the second field is true iff it is a function ctor
pub struct Ctor {
    pub of: CtorOf,
    pub is_fn_ctor: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum DefKind {
    Mod,
    Struct,
    Union,
    Enum,
    Variant,
    Trait,
    TyAlias,
    ForeignTy,
    TraitAlias,
    AssocTy,
    TyParam,
    Fn,
    Const,
    ConstParam,
    Static {
        safety: bool,
        mutability: bool,
        nested: bool,
    },
    Ctor {
        ctor: Ctor,
    },
    AssocFn,
    AssocConst,
    Macro {
        kind: MacroKind,
    },
    ExternCrate,
    Use,
    ForeignMod,
    AnonConst,
    InlineConst,
    OpaqueTy,
    Field,
    LifetimeParam,
    GlobalAsm,
    Impl {
        of_trait: bool,
    },
    Closure,
    SyntheticCoroutineBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum NonMacroAttrKind {
    Builtin(Symbol),
    Tool,
    DeriveHelper,
    DeriveHelperCompat,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum PrimHirTy {
    Int { ty: IntTy },
    Uint { ty: UintTy },
    Float { ty: FloatTy },
    Str,
    Bool,
    Char,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum FloatTy {
    F16,
    F32,
    F64,
    F128,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum CtorOf {
    Struct,
    Variant,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum MacroKind {
    /// A bang macro `foo!()`.
    Bang,
    /// An attribute macro `#[foo]`.
    Attr,
    /// A derive macro `#[derive(Foo)]`
    Derive,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PathSegment {
    pub ident: Ident,
    pub hir_id: HirId,
    pub res: Res,
    pub args: Option<GenericArgs>,
    pub infer_args: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct GenericArgs {
    pub args: Vec<GenericArg>,
    pub constraints: Vec<AssocItemConstraint>,
    pub parenthesized: GenericArgsParentheses,
    pub span_ext: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum GenericArg {
    Lifetime { lifetime: Lifetime },
    Type { ty: HirTy },
    Const { c: ConstArg },
    Infer { infer: InferArg },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct InferArg {
    pub hir_id: HirId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum GenericArgsParentheses {
    No,
    ReturnTypeNotation,
    ParenSugar,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AssocItemConstraint {
    pub hir_id: HirId,
    pub ident: Ident,
    pub gen_args: GenericArgs,
    pub kind: AssocItemConstraintKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum AssocItemConstraintKind {
    Equality { term: Term },
    Bound { bounds: Vec<GenericBound> },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum GenericBound {
    Trait {
        trait_ref: PolyTraitRef,
    },
    Outlives {
        lifetime: Lifetime,
    },
    Use {
        args: Vec<PreciseCapturingArg>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PolyTraitRef {
    pub bound_generic_params: Vec<GenericParam>,
    pub trait_ref: TraitRef,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct GenericParam {
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    pub name: ParamName,
    pub span: Span,
    pub pure_wrt_drop: bool,
    pub kind: GenericParamKind,
    pub colon_span: Option<Span>,
    pub source: GenericParamSource,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum GenericParamSource {
    Generics,
    Binder,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum ParamName {
    Plain { ident: Ident },
    Fresh,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum GenericParamKind {
    Lifetime {
        kind: LifetimeParamKind,
    },
    Type {
        default: Option<HirTy>,
        synthetic: bool,
    },
    Const {
        ty: HirTy,
        default: Option<ConstArg>,
        synthetic: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum LifetimeParamKind {
    Explicit,
    Elided { kind: MissingLifetimeKind },
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum MissingLifetimeKind {
    Underscore,
    Ampersand,
    Comma,
    Brackets,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TraitRef {
    pub path: Path,
    pub hir_ref_id: HirId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum PreciseCapturingArg {
    Lifetime { lifetime: Lifetime },
    Param { arg: PreciseCapturingNonLifetimeArg },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Lifetime {
    pub hir_id: HirId,
    pub ident: Ident,
    pub kind: LifetimeKind,
    pub source: LifetimeSource,
    pub syntax: LifetimeSyntax,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PreciseCapturingNonLifetimeArg {
    pub hir_id: HirId,
    pub ident: Ident,
    pub res: Res,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum LifetimeKind {
    Param { id: LocalDefId },
    ImplicitObjectLifetimeDefault,
    Error,
    Infer,
    Static,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum LifetimeSource {
    Reference,
    Path { angle_brackets: AngleBrackets },
    OutlivesBound,
    PreciseCapturing,
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum AngleBrackets {
    Missing,
    Empty,
    Full,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum LifetimeSyntax {
    Hidden,
    Anonymous,
    Named,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum Term {
    Ty { ty: HirTy },
    Const { c: ConstArg },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ConstArg {
    pub hir_id: HirId,
    pub kind: ConstArgKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum ConstArgKind {
    Path { path: QPath },
    Anon { ac: AnonConst },
    Infer { span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AnonConst {
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    #[serde(skip)]
    pub body_id: rustc_hir::BodyId,
    pub body: Body,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum QPath {
    Resolved { ty: Option<HirTy>, path: Path },
    TypeRelative { ty: HirTy, seg: PathSegment },
    LangItem { item: LangItem, span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BodyId {
    pub hir_id: HirId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum LangItem {
    Sized,
    Unsize,
    StructuralPeq,
    Copy,
    Clone,
    CloneFn,
    UseCloned,
    Sync,
    DiscriminantKind,
    Discriminant,
    PointeeTrait,
    Metadata,
    DynMetadata,
    Freeze,
    UnsafeUnpin,
    FnPtrTrait,
    FnPtrAddr,
    Drop,
    Destruct,
    AsyncDrop,
    AsyncDropInPlace,
    CoerceUnsized,
    DispatchFromDyn,
    TransmuteOpts,
    TransmuteTrait,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    Not,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitXorAssign,
    BitAndAssign,
    BitOrAssign,
    ShlAssign,
    ShrAssign,
    Index,
    IndexMut,
    UnsafeCell,
    UnsafePinned,
    VaList,
    Deref,
    DerefMut,
    DerefPure,
    DerefTarget,
    Receiver,
    ReceiverTarget,
    LegacyReceiver,
    Fn,
    FnMut,
    FnOnce,
    AsyncFn,
    AsyncFnMut,
    AsyncFnOnce,
    AsyncFnOnceOutput,
    CallOnceFuture,
    CallRefFuture,
    AsyncFnKindHelper,
    AsyncFnKindUpvars,
    FnOnceOutput,
    Iterator,
    FusedIterator,
    Future,
    FutureOutput,
    AsyncIterator,
    CoroutineState,
    Coroutine,
    CoroutineReturn,
    CoroutineYield,
    CoroutineResume,
    Unpin,
    Pin,
    OrderingEnum,
    PartialEq,
    PartialOrd,
    CVoid,
    Panic,
    PanicNounwind,
    PanicFmt,
    ConstPanicFmt,
    PanicBoundsCheck,
    PanicMisalignedPointerDereference,
    PanicInfo,
    PanicLocation,
    PanicImpl,
    PanicCannotUnwind,
    PanicInCleanup,
    PanicAddOverflow,
    PanicSubOverflow,
    PanicMulOverflow,
    PanicDivOverflow,
    PanicRemOverflow,
    PanicNegOverflow,
    PanicShrOverflow,
    PanicShlOverflow,
    PanicDivZero,
    PanicRemZero,
    PanicCoroutineResumed,
    PanicAsyncFnResumed,
    PanicAsyncGenFnResumed,
    PanicGenFnNone,
    PanicCoroutineResumedPanic,
    PanicAsyncFnResumedPanic,
    PanicAsyncGenFnResumedPanic,
    PanicGenFnNonePanic,
    PanicNullPointerDereference,
    PanicCoroutineResumedDrop,
    PanicAsyncFnResumedDrop,
    PanicAsyncGenFnResumedDrop,
    PanicGenFnNoneDrop,
    BeginPanic,
    FormatArgument,
    FormatArguments,
    FormatCount,
    FormatPlaceholder,
    FormatUnsafeArg,
    ExchangeMalloc,
    DropInPlace,
    AllocLayout,
    Start,
    EhPersonality,
    EhCatchTypeinfo,
    OwnedBox,
    GlobalAlloc,
    PtrUnique,
    PhantomData,
    ManuallyDrop,
    BikeshedGuaranteedNoDrop,
    MaybeUninit,
    Termination,
    Try,
    Tuple,
    SliceLen,
    TryTraitFromResidual,
    TryTraitFromOutput,
    TryTraitBranch,
    TryTraitFromYeet,
    PointerLike,
    CoercePointeeValidated,
    ConstParamTy,
    UnsizedConstParamTy,
    Poll,
    PollReady,
    PollPending,
    AsyncGenReady,
    AsyncGenPending,
    AsyncGenFinished,
    ResumeTy,
    GetContext,
    Context,
    FuturePoll,
    AsyncIteratorPollNext,
    IntoAsyncIterIntoIter,
    Option,
    OptionSome,
    OptionNone,
    ResultOk,
    ResultErr,
    ControlFlowContinue,
    ControlFlowBreak,
    IntoFutureIntoFuture,
    IntoIterIntoIter,
    IteratorNext,
    PinNewUnchecked,
    RangeFrom,
    RangeFull,
    RangeInclusiveStruct,
    RangeInclusiveNew,
    Range,
    RangeToInclusive,
    RangeTo,
    RangeMax,
    RangeMin,
    RangeSub,
    RangeFromCopy,
    RangeCopy,
    RangeInclusiveCopy,
    String,
    CStr,
    ContractBuildCheckEnsures,
    ContractCheckRequires,
    DefaultTrait4,
    DefaultTrait3,
    DefaultTrait2,
    DefaultTrait1,
    ContractCheckEnsures,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}
