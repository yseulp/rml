use std::rc::Rc;

use serde::{Deserialize, Serialize};

use wrappers::{LocalDefIdWrapper, SpanWrapper};

use self::wrappers::{DefIdWrapper, HirIdWrapper, IdentWrapper, ItemIdWrapper, SymbolWrapper};

mod serialize;
pub mod translation;
pub mod wrappers;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Term {
    pub hir_id: HirIdWrapper,
    pub kind: TermKind,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermKind {
    Array(Vec<Term>),
    Call(Box<Term>, Vec<Term>),
    MethodCall(TermPathSegment, Box<Term>, Vec<Term>, SpanWrapper),
    Tup(Vec<Term>),
    Binary(TermBinOp, Box<Term>, Box<Term>),
    Unary(TermUnOp, Box<Term>),
    Lit(TermLit),
    Cast(Box<Term>, TermTy),
    Let(TermLet),
    If(Box<Term>, Box<Term>, Option<Box<Term>>),
    Match(Box<Term>, Vec<TermArm>, TermMatchSource),
    Closure(TermClosure),
    Block(TermBlock),
    Field(Box<Term>, IdentWrapper),
    Index(Box<Term>, Box<Term>),
    Path(TermQPath),
    AddrOf(TermBorrowKind, TermMutability, Box<Term>),
    Struct(TermQPath, Vec<TermField>, Option<Box<Term>>),
    Repeat(Box<Term>, Box<TermArrayLen>),

    // RML special kinds
    Quantor(QuantorKind, QuantorParam, Box<Term>),
    Model(ModelKind, Box<Term>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ModelKind {
    Shallow,
    Deep,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermBinOp {
    node: TermBinOpKind,
    span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermUnOp {
    Deref,
    Not,
    Neg,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermBinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,

    // RML
    LogEq,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermLit {
    pub node: TermLitKind,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermLitKind {
    Str(SymbolWrapper, TermStrStyle),
    ByteStr(Rc<[u8]>, TermStrStyle),
    CStr(Rc<[u8]>, TermStrStyle),
    Byte(u8),
    Char(char),
    Int(u128, TermLitIntType),
    Float(SymbolWrapper, TermLitFloatType),
    Bool(bool),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermLitIntType {
    Signed(TermIntTy),
    Unsigned(TermUintTy),
    Unsuffixed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermLitFloatType {
    Suffixed(TermFloatTy),
    Unsuffixed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermStrStyle {
    Cooked,
    Raw(u8),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermBorrowKind {
    Ref,
    Raw,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermMatchSource {
    Normal,
    ForLoopDesugar,
    TryDesugar,
    AwaitDesugar,
    FormatArgs,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermLet {
    pub hir_id: HirIdWrapper,
    pub span: SpanWrapper,
    pub pat: TermPat,
    pub ty: Option<TermTy>,
    pub init: Box<Term>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermArm {
    pub hir_id: HirIdWrapper,
    pub span: SpanWrapper,
    pub pat: TermPat,
    pub guard: Option<TermGuard>,
    pub body: Box<Term>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermClosure {
    pub def_id: LocalDefIdWrapper,
    pub binder: TermClosureBinder,
    pub constness: TermConstness,
    pub capture_clause: TermCaptureBy,
    pub bound_generic_params: Vec<TermGenericParam>,
    pub fn_decl: TermFnDecl,
    pub body: TermBody,
    pub fn_decl_span: SpanWrapper,
    pub fn_arg_span: Option<SpanWrapper>,
    pub movability: Option<TermMovability>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermClosureBinder {
    Default,
    For { span: SpanWrapper },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermConstness {
    Const,
    NotConst,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermCaptureBy {
    Value,
    Ref,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermMovability {
    Static,
    Movable,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermBlock {
    pub stmts: Vec<TermStmt>,
    pub term: Option<Box<Term>>,
    pub hir_id: HirIdWrapper,
    pub rules: TermBlockCheckMode,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermBlockCheckMode {
    DefaultBlock,
    UnsafeBlock(TermUnsafeSource),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermUnsafeSource {
    CompilerGenerated,
    UserProvided,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermGuard {
    If(Box<Term>),
    IfLet(TermLet),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermStmt {
    pub hir_id: HirIdWrapper,
    pub kind: TermStmtKind,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermStmtKind {
    Local(TermLocal),
    Item(ItemIdWrapper),
    Term(Box<Term>),
    Semi(Box<Term>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermLocal {
    pub pat: TermPat,
    pub ty: Option<TermTy>,
    pub init: Option<Box<Term>>,
    pub hir_id: HirIdWrapper,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermField {
    pub hir_id: HirIdWrapper,
    pub ident: IdentWrapper,
    pub term: Term,
    pub span: SpanWrapper,
    pub is_shorthand: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermBody {
    pub params: Vec<TermParam>,
    pub value: Box<Term>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermParam {
    pub hir_id: HirIdWrapper,
    pub pat: TermPat,
    pub ty_span: SpanWrapper,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermPat {
    pub hir_id: HirIdWrapper,
    pub kind: TermPatKind,
    pub span: SpanWrapper,
    pub default_binding_modes: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermPatKind {
    Wild,
    Binding(
        TermBindingAnnotation,
        HirIdWrapper,
        IdentWrapper,
        Option<Box<TermPat>>,
    ),
    Struct(TermQPath, Vec<TermPatField>, bool),
    TupleStruct(TermQPath, Vec<TermPat>, TermDotDotPos),
    Or(Vec<TermPat>),
    Path(TermQPath),
    Tuple(Vec<TermPat>, TermDotDotPos),
    Box(Box<TermPat>),
    Ref(Box<TermPat>, TermMutability),
    Lit(Box<Term>),
    Range(Option<Box<Term>>, Option<Box<Term>>, TermRangeEnd),
    Slice(Vec<TermPat>, Option<Box<TermPat>>, Vec<TermPat>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermDotDotPos(pub u32);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermRangeEnd {
    Included,
    Excluded,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermBindingAnnotation(pub TermByRef, pub TermMutability);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermByRef {
    Yes,
    No,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermPatField {
    pub hir_id: HirIdWrapper,
    pub ident: IdentWrapper,
    pub pat: TermPat,
    pub is_shorthand: bool,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QuantorKind {
    Exists,
    Forall,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuantorParam {
    pub hir_id: HirIdWrapper,
    pub ident: IdentWrapper,
    pub ty: TermTy,
    pub span: SpanWrapper,
    pub ty_span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermPathSegment {
    pub ident: IdentWrapper,
    pub hir_id: HirIdWrapper,
    pub res: TermRes,
    pub args: Option<TermGenericArgs>,
    pub infer_args: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermTypeBinding {
    pub hir_id: HirIdWrapper,
    pub ident: IdentWrapper,
    pub gen_args: TermGenericArgs,
    pub kind: TermTypeBindingKind,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermTypeBindingKind {
    Constraint { bounds: Vec<TermGenericBound> },
    Equality { hir_term: HirTerm },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirTerm {
    Ty(TermTy),
    Const(TermAnonConst),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermGenericBound {
    Trait(TermPolyTraitRef, TermTraitBoundModifier),
    LangItemTrait(TermLangItem, SpanWrapper, HirIdWrapper, TermGenericArgs),
    Outlives(TermLifetime),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermTraitBoundModifier {
    None,
    Negative,
    Maybe,
    MaybeConst,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermGenericArgs {
    pub args: Vec<TermGenericArg>,
    pub bindings: Vec<TermTypeBinding>,
    pub parenthesized: TermGenericArgsParentheses,
    pub span_ext: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermGenericArgsParentheses {
    No,
    ReturnTypeNotation,
    ParenSugar,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermGenericArg {
    Lifetime(TermLifetime),
    Type(TermTy),
    Const(TermConstArg),
    Infer(TermInferArg),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermConstArg {
    pub value: TermAnonConst,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermInferArg {
    pub hir_id: HirIdWrapper,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermLifetime {
    pub hir_id: HirIdWrapper,
    pub ident: IdentWrapper,
    pub res: TermLifetimeName,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermLifetimeName {
    Param(LocalDefIdWrapper),
    ImplicitObjectLifetimeDefault,
    Infer,
    Static,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermTy {
    pub hir_id: HirIdWrapper,
    pub kind: TermTyKind,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermTyKind {
    Slice(Box<TermTy>),
    Array(Box<TermTy>, Box<TermArrayLen>),
    Ptr(Box<TermMutTy>),
    Ref(TermLifetime, Box<TermMutTy>),
    BareFn(TermBareFnTy),
    Never,
    Tup(Vec<TermTy>),
    Path(TermQPath),
    OpaqueDef(ItemIdWrapper, Vec<TermGenericArg>, bool),
    TraitObject(Vec<TermPolyTraitRef>, TermLifetime, TermTraitObjectSyntax),
    Typeof(Box<TermAnonConst>),
    Infer,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermArrayLen {
    Infer(HirIdWrapper, SpanWrapper),
    Body(TermAnonConst),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermBareFnTy {
    pub abi: TermAbi,
    pub generic_params: Vec<TermGenericParam>,
    pub decl: TermFnDecl,
    pub param_names: Vec<IdentWrapper>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermFnDecl {
    pub inputs: Vec<TermTy>,
    pub output: TermFnRetTy,
    pub c_variadic: bool,
    pub implicit_self: TermImplicitSelfKind,
    pub lifetime_elision_allowed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermFnRetTy {
    DefaultReturn(SpanWrapper),
    Return(Box<TermTy>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermImplicitSelfKind {
    Imm,
    Mut,
    ImmRef,
    MutRef,
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermAbi {
    Rust,
    C { unwind: bool },
    Cdecl { unwind: bool },
    Stdcall { unwind: bool },
    Fastcall { unwind: bool },
    Vectorcall { unwind: bool },
    Thiscall { unwind: bool },
    Aapcs { unwind: bool },
    Win64 { unwind: bool },
    SysV64 { unwind: bool },
    PtxKernel,
    Msp430Interrupt,
    X86Interrupt,
    AmdGpuKernel,
    EfiApi,
    AvrInterrupt,
    AvrNonBlockingInterrupt,
    CCmseNonSecureCall,
    Wasm,
    System { unwind: bool },
    RustIntrinsic,
    RustCall,
    PlatformIntrinsic,
    Unadjusted,
    RustCold,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermMutTy {
    pub ty: TermTy,
    pub mutbl: TermMutability,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermAnonConst {
    pub hir_id: HirIdWrapper,
    pub def_id: LocalDefIdWrapper,
    pub body: Term,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermMutability {
    Not,
    Mut,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermTraitObjectSyntax {
    Dyn,
    DynStar,
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermTraitRef {
    pub path: TermPath,
    pub hir_ref_id: HirIdWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermPath<R = TermRes> {
    pub span: SpanWrapper,
    pub res: R,
    pub segments: Vec<TermPathSegment>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermGenericParam {
    pub hir_id: HirIdWrapper,
    pub def_id: LocalDefIdWrapper,
    pub name: TermParamName,
    pub span: SpanWrapper,
    pub pure_wrt_drop: bool,
    pub kind: TermGenericParamKind,
    pub colon_span: Option<SpanWrapper>,
    pub source: TermGenericParamSource,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermParamName {
    Plain(IdentWrapper),
    Fresh,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermGenericParamKind {
    Lifetime {
        kind: TermLifetimeParamKind,
    },
    Type {
        default: Option<TermTy>,
        synthetic: bool,
    },
    Const {
        ty: TermTy,
        default: Option<TermAnonConst>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermLifetimeParamKind {
    Explicit,
    Elided,
    Error,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermGenericParamSource {
    Generics,
    Binder,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermQPath {
    Resolved(Option<Box<TermTy>>, TermPath),
    TypeRelative(Box<TermTy>, TermPathSegment),
    LangItem(TermLangItem, SpanWrapper, Option<HirIdWrapper>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TermPolyTraitRef {
    pub bound_generic_params: Vec<TermGenericParam>,
    pub trait_ref: TermTraitRef,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermRes<Id = HirIdWrapper> {
    Def(TermDefKind, DefIdWrapper),
    PrimTy(TermPrimTy),
    SelfTyParam {
        trait_: DefIdWrapper,
    },
    SelfTyAlias {
        alias_to: DefIdWrapper,
        forbid_generic: bool,
        is_trait_impl: bool,
    },
    SelfCtor(DefIdWrapper),
    Local(Id),
    ToolMod,
    NonMacroAttr(TermNonMacroAttrKind),
    Err,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermNonMacroAttrKind {
    Builtin(SymbolWrapper),
    Tool,
    DeriveHelper,
    DeriveHelperCompat,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermDefKind {
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
    Static(TermMutability),
    Ctor(TermCtorOf, TermCtorKind),
    AssocFn,
    AssocConst,
    Macro(TermMacroKind),
    ExternCrate,
    Use,
    ForeignMod,
    AnonConst,
    InlineConst,
    OpaqueTy,
    ImplTraitPlaceholder,
    Field,
    LifetimeParam,
    GlobalAsm,
    Impl { of_trait: bool },
    Closure,
    Generator,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermPrimTy {
    Int(TermIntTy),
    Uint(TermUintTy),
    Float(TermFloatTy),
    Str,
    Bool,
    Char,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermIntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermUintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermFloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermCtorOf {
    Struct,
    Variant,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermCtorKind {
    Fn,
    Const,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermMacroKind {
    Bang,
    Attr,
    Derive,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TermLangItem {
    Sized,
    Unsize,
    StructuralPeq,
    StructuralTeq,
    Copy,
    Clone,
    Sync,
    DiscriminantKind,
    Discriminant,
    PointeeTrait,
    Metadata,
    DynMetadata,
    Freeze,
    FnPtrTrait,
    FnPtrAddr,
    Drop,
    Destruct,
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
    VaList,
    Deref,
    DerefMut,
    DerefTarget,
    Receiver,
    Fn,
    FnMut,
    FnOnce,
    FnOnceOutput,
    Future,
    GeneratorState,
    Generator,
    Unpin,
    Pin,
    PartialEq,
    PartialOrd,
    CVoid,
    Panic,
    PanicNounwind,
    PanicFmt,
    PanicDisplay,
    ConstPanicFmt,
    PanicBoundsCheck,
    PanicMisalignedPointerDereference,
    PanicInfo,
    PanicLocation,
    PanicImpl,
    PanicCannotUnwind,
    BeginPanic,
    FormatAlignment,
    FormatArgument,
    FormatArguments,
    FormatCount,
    FormatPlaceholder,
    FormatUnsafeArg,
    ExchangeMalloc,
    BoxFree,
    DropInPlace,
    AllocLayout,
    Start,
    EhPersonality,
    EhCatchTypeinfo,
    OwnedBox,
    PtrUnique,
    PhantomData,
    ManuallyDrop,
    MaybeUninit,
    AlignOffset,
    Termination,
    Try,
    Tuple,
    SliceLen,
    TryTraitFromResidual,
    TryTraitFromOutput,
    TryTraitBranch,
    TryTraitFromYeet,
    PointerLike,
    ConstParamTy,
    Poll,
    PollReady,
    PollPending,
    ResumeTy,
    GetContext,
    Context,
    FuturePoll,
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
    String,
    CStr,
}
