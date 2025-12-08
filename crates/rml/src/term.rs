//! Collect expressions that are actually terms encoded as expressions.

use std::sync::Arc;

use serde::Serialize;

use crate::hir::{DefId, HirId, Ident, ItemId, LocalDefId, Span, Symbol};

pub mod translation;

/// A term extracted from an expression that is an encoded term from rml_syn.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct Term {
    /// Id of the original expression.
    pub hir_id: HirId,
    /// Kind of the term.
    pub kind: TermKind,
    /// Span of the original expression.
    pub span: Span,
}

/// Kind of the [Term].
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermKind {
    /// Array term, e.g., `[e1, e2, ..., eN]`.
    Array { terms: Vec<Term> },
    /// A call, e.g., `f(a1, ..., aN)`.
    Call { callee: Box<Term>, args: Vec<Term> },
    /// A method call, e.g., `x.foo::<'static, Bar, Baz>(a, b, c, d)`.
    ///
    /// The [TermPathSegment] is the path of the method, [Term] is the callee,
    /// `Vec<Term>` are the arguments, and [Span] is the original
    /// expressions span.
    MethodCall {
        path: TermPathSegment,
        callee: Box<Term>,
        args: Vec<Term>,
        span: Span,
    },
    /// A tuple, e.g., `(e1, ..., eN)`.
    Tup { terms: Vec<Term> },
    /// A binary term, e.g., `e1 + eN`.
    Binary {
        op: TermBinOp,
        left: Box<Term>,
        right: Box<Term>,
    },
    /// A unary term, e.g., `!e`.
    Unary { op: TermUnOp, child: Box<Term> },
    /// A term literal, e.g., `1`.
    Lit { lit: TermLit },
    /// A cast term, e.g., `foo as f64`.
    Cast { term: Box<Term>, ty: TermTy },
    /// A let `$pat = $term` term.
    ///
    /// These are not `Local` and only occur as terms. The `let Some(x) = foo()`
    /// in `if let Some(x) = foo()` is an example of `Let(..)`.
    Let { r#let: TermLet },
    /// An if block, with an optional else block.
    ///
    /// I.e., `if <term> { <term> } else { <term> }`.
    If {
        cond: Box<Term>,
        then: Box<Term>,
        r#else: Option<Box<Term>>,
    },
    /// A `match` block, with a source that indicates whether or not it is the
    /// result of a desugaring, and if so, which kind.
    Match {
        term: Box<Term>,
        arms: Vec<TermArm>,
        src: TermMatchSource,
    },
    /// A closure (e.g., `|a, b, c| {a + b + c}`).
    Closure { closure: TermClosure },
    /// A block (e.g., `{ ... }`).
    Block { block: TermBlock },
    /// Access of a named (e.g., `obj.foo`) or unnamed (e.g., `obj.0`) struct or
    /// tuple field.
    Field { term: Box<Term>, field: Ident },
    /// An indexing operation (`foo[2]`). Similar to [TermKind::MethodCall], the
    /// final [Span] represents the span of the brackets and index.
    Index {
        term: Box<Term>,
        idx: Box<Term>,
        span: Span,
    },
    /// Path to a definition, possibly containing lifetime or type parameters.
    Path { path: TermQPath },
    /// A referencing operation (i.e., `&a` or `&mut a`).
    ///
    /// TODO: Remove?
    AddrOf {
        kind: TermBorrowKind,
        mutability: TermMutability,
        term: Box<Term>,
    },
    /// A struct or struct-like variant literal term.
    ///
    /// E.g., `Foo {x: 1, y: 2}`, or `Foo {x: 1, .. base}`, where `base` is the
    /// `Option<Expr>`.
    Struct {
        path: TermQPath,
        fields: Vec<TermField>,
        rest: StructTailTerm,
    },
    /// An array literal constructed from one repeated element.
    ///
    /// E.g., `[1; 5]`. The first term is the element to be repeated; the
    /// second is the number of times to repeat it.
    Repeat {
        term: Box<Term>,
        len: Box<TermConstArg>,
    },

    // RML special kinds
    /// A quantor like `forall(|x: int| x > 0)`.
    Quantor {
        kind: QuantorKind,
        param: QuantorParam,
        term: Box<Term>,
    },
    /// A model term, e.g., `x@`.
    Model { kind: ModelKind, term: Box<Term> },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum StructTailTerm {
    /// A struct expression where all the fields are explicitly enumerated: `Foo
    /// { a, b }`.
    None,
    /// A struct expression with a "base", an expression of the same type as the
    /// outer struct that will be used to populate any fields not explicitly
    /// mentioned: `Foo { ..base }`
    Base(Box<Term>),
    /// A struct expression with a `..` tail but no "base" expression. The
    /// values from the struct fields' default values will be used to
    /// populate any fields not explicitly mentioned: `Foo { .. }`.
    DefaultFields(Span),
}

/// The kind of a model term.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum ModelKind {
    /// A shallow model, i.e., type parameters are _not_ altered.
    ///
    /// E.g.: A shallow model of `Vec<T>` is `Seq<T>`.
    Shallow,
    /// A deep model, i.e., type parameters are transformed to their deep model.
    ///
    /// E.g.: A deep model of `Vec<T>` is `Seq<T::DeepModel>`.
    Deep,
}

/// A binary operator on terms.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermBinOp {
    node: TermBinOpKind,
    span: Span,
}

/// A unary operator on terms.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermUnOp {
    Deref,
    Not,
    Neg,
}
/// Different binary operator kinds.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermBinOpKind {
    /// `a + b`.
    Add,
    /// `a - b`.
    Sub,
    /// `a * b`.
    Mul,
    /// `a / b`.
    Div,
    /// `a % b`.
    Rem,
    /// `a && b`.
    And,
    /// `a || b`.
    Or,
    /// `a ^ b`.
    BitXor,
    /// `a & b`.
    BitAnd,
    /// `a | b`.
    BitOr,
    /// `a << b`.
    Shl,
    /// `a >> b`.
    Shr,
    /// `a == b`.
    Eq,
    /// `a < b`.
    Lt,
    /// `a <= b`.
    Le,
    /// `a != b`.
    Ne,
    /// `a > b`.
    Ge,
    /// `a >= b`.
    Gt,

    /// RML-specific, logical equality, irrespective of implementations of [Eq].
    ///
    /// `a === b`.
    LogEq,
    /// `a ==> b`.
    Implication,
}

/// A literal.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermLit {
    pub node: TermLitKind,
    pub span: Span,
}

/// Kinds of term literal.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermLitKind {
    /// String literal, e.g., `"hello"`.
    Str { symbol: Symbol, style: TermStrStyle },
    /// Byte string literal, e.g., `b"hello"`.
    ByteStr {
        bytes: Arc<[u8]>,
        style: TermStrStyle,
    },
    /// C-string literal.
    CStr {
        bytes: Arc<[u8]>,
        style: TermStrStyle,
    },
    /// A raw byte.
    Byte { value: u8 },
    /// A char `'h'`.
    Char { value: char },
    /// Integer literal and its bitsize, e.g., 42u128.
    Int { value: u128, ty: TermLitIntType },
    /// Float literal, e.g., `42.5f64`.
    Float {
        symbol: Symbol,
        ty: TermLitFloatType,
    },
    /// Bool literal, i.e., `true` or `false`.
    Bool { value: bool },
}

/// Integer literal type.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermLitIntType {
    /// `i8`, `i16`, ...
    Signed { ty: TermIntTy },
    /// `u8`, `u16`, ...
    Unsigned { ty: TermUintTy },
    /// No suffix.
    Unsuffixed,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermLitFloatType {
    Suffixed { ty: TermFloatTy },
    Unsuffixed,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermStrStyle {
    Cooked,
    Raw { number: u8 },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermBorrowKind {
    Ref,
    Raw,
}

/// Matches may be declared by the user or are created when lowering the AST to
/// HIR. This enum denotes whether this is the former
/// ([TermMatchSource::Normal]) or the latter (including where it comes from).
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermMatchSource {
    Normal,
    Postfix,
    ForLoopDesugar,
    TryDesugar(HirId),
    AwaitDesugar,
    FormatArgs,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermLet {
    pub span: Span,
    pub pat: TermPat,
    pub ty: Option<TermTy>,
    pub init: Box<Term>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermArm {
    pub hir_id: HirId,
    pub span: Span,
    pub pat: TermPat,
    pub guard: Option<Term>,
    pub body: Box<Term>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermClosure {
    pub def_id: LocalDefId,
    pub binder: TermClosureBinder,
    pub constness: TermConstness,
    pub capture_clause: TermCaptureBy,
    pub bound_generic_params: Vec<TermGenericParam>,
    pub fn_decl: TermFnDecl,
    pub body: TermBody,
    pub fn_decl_span: Span,
    pub fn_arg_span: Option<Span>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermClosureBinder {
    Default,
    For { span: Span },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermConstness {
    Const,
    NotConst,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermCaptureBy {
    Value { move_kw: Span },
    Ref,
    Use { use_kw: Span },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermBlock {
    pub stmts: Vec<TermStmt>,
    pub term: Option<Box<Term>>,
    pub hir_id: HirId,
    pub rules: TermBlockCheckMode,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermBlockCheckMode {
    DefaultBlock,
    UnsafeBlock(TermUnsafeSource),
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermUnsafeSource {
    CompilerGenerated,
    UserProvided,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermStmt {
    pub hir_id: HirId,
    pub kind: TermStmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermStmtKind {
    Let { r#let: TermLetStmt },
    Item { id: ItemId },
    Term { term: Box<Term> },
    Semi { term: Box<Term> },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermLetStmt {
    pub pat: TermPat,
    pub ty: Option<TermTy>,
    pub init: Box<Term>,
    pub hir_id: HirId,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermField {
    pub hir_id: HirId,
    pub ident: Ident,
    pub term: Term,
    pub span: Span,
    pub is_shorthand: bool,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermBody {
    pub params: Vec<TermParam>,
    pub value: Box<Term>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermParam {
    pub hir_id: HirId,
    pub pat: TermPat,
    pub ty_span: Span,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermPat {
    pub hir_id: HirId,
    pub kind: TermPatKind,
    pub span: Span,
    pub default_binding_modes: bool,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermPatKind {
    Wild,
    Missing,
    Term {
        term: PatTerm,
    },
    Guard {
        pat: Box<TermPat>,
        guard: Box<Term>,
    },
    Binding {
        mode: TermBindingMode,
        hir_id: HirId,
        ident: Ident,
        pat: Option<Box<TermPat>>,
    },
    Struct {
        path: TermQPath,
        fields: Vec<TermPatField>,
        rest: bool,
    },
    TupleStruct {
        path: TermQPath,
        pats: Vec<TermPat>,
        dot_dot_pos: TermDotDotPos,
    },
    Or {
        pats: Vec<TermPat>,
    },
    Path {
        path: TermQPath,
    },
    Tuple {
        pats: Vec<TermPat>,
        dot_dot_pos: TermDotDotPos,
    },
    Box {
        pat: Box<TermPat>,
    },
    Ref {
        pat: Box<TermPat>,
        mutability: TermMutability,
    },
    Lit {
        term: Box<Term>,
    },
    Range {
        lhs: Option<Box<PatTerm>>,
        rhs: Option<Box<PatTerm>>,
        range: TermRangeEnd,
    },
    Slice {
        start: Vec<TermPat>,
        mid: Option<Box<TermPat>>,
        rest: Vec<TermPat>,
    },
    Never,
    Deref {
        pat: Box<TermPat>,
    },
    Err,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct PatTerm {
    pub hir_id: HirId,
    pub span: Span,
    pub kind: PatTermKind,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum PatTermKind {
    Lit {
        lit: TermLit,
        // FIXME: move this into `Lit` and handle negated literal expressions
        // once instead of matching on unop neg expressions everywhere.
        negated: bool,
    },
    ConstBlock(TermConstBlock),
    /// A path pattern for a unit struct/variant or a (maybe-associated)
    /// constant.
    Path(TermQPath),
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermConstBlock {
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    pub body: TermBody,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermDotDotPos(pub u32);

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermRangeEnd {
    Included,
    Excluded,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermBindingMode {
    pub by_ref: TermByRef,
    pub r#mut: TermMutability,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermByRef {
    Yes { r#mut: bool },
    No,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermPatField {
    pub hir_id: HirId,
    pub ident: Ident,
    pub pat: TermPat,
    pub is_shorthand: bool,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
pub enum QuantorKind {
    Exists,
    Forall,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct QuantorParam {
    pub hir_id: HirId,
    pub ident: Ident,
    pub ty: TermTy,
    pub span: Span,
    pub ty_span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermPathSegment {
    pub ident: Ident,
    pub hir_id: HirId,
    pub res: TermRes,
    pub args: Option<TermGenericArgs>,
    pub infer_args: bool,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermTypeBinding {
    pub hir_id: HirId,
    pub ident: Ident,
    pub gen_args: TermGenericArgs,
    pub kind: TermTypeBindingKind,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermTypeBindingKind {
    Constraint { bounds: Vec<TermGenericBound> },
    Equality { hir_term: HirTerm },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum HirTerm {
    Ty(TermTy),
    Const(TermConstArg),
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermGenericBound {
    Trait(TermPolyTraitRef),
    Outlives(TermLifetime),
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermTraitBoundModifier {
    None,
    Negative,
    Maybe,
    MaybeConst,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermGenericArgs {
    pub args: Vec<TermGenericArg>,
    // pub constraints: ,
    pub parenthesized: TermGenericArgsParentheses,
    pub span_ext: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermGenericArgsParentheses {
    No,
    ReturnTypeNotation,
    ParenSugar,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermGenericArg {
    Lifetime(TermLifetime),
    Type(TermTy),
    Const(TermConstArg),
    Infer(TermInferArg),
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermConstArg {
    pub hir_id: HirId,
    pub kind: TermConstArgKind,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermConstArgKind {
    Path(TermQPath),
    Anon(TermAnonConst),
    Infer,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermInferArg {
    pub hir_id: HirId,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermLifetime {
    pub hir_id: HirId,
    pub ident: Ident,
    pub kind: TermLifetimeKind,
    pub source: TermLifetimeSource,
    pub syntax: TermLifetimeSyntax,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermLifetimeKind {
    Param(LocalDefId),
    ImplicitObjectLifetimeDefault,
    Infer,
    Static,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermLifetimeSource {
    Reference,
    Path { angle_brackets: TermAngleBrackets },
    OutlivesBound,
    PreciseCapturing,
    Other,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermAngleBrackets {
    Missing,
    Empty,
    Full,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermLifetimeSyntax {
    Hidden,
    Anonymous,
    Named,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermTy {
    pub hir_id: HirId,
    pub kind: TermTyKind,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermTyKind {
    Slice {
        ty: Box<TermTy>,
    },
    Array {
        ty: Box<TermTy>,
        len: Box<TermConstArg>,
    },
    Ptr {
        ty: Box<TermMutTy>,
    },
    Ref {
        lifetime: TermLifetime,
        ty: Box<TermMutTy>,
    },
    BareFn {
        ty: TermBareFnTy,
    },
    Never,
    Tup {
        tys: Vec<TermTy>,
    },
    Path {
        path: TermQPath,
    },
    OpaqueDef {
        ty: TermOpaqueTy,
    },
    TraitObject {
        refs: Vec<TermPolyTraitRef>,
        lifetime: TermLifetime,
        syntax: TermTraitObjectSyntax,
    },
    Typeof {
        r#const: Box<TermAnonConst>,
    },
    InferDelegation {
        def_id: DefId,
    },
    AnonAdt {
        id: ItemId,
    },
    Pat {
        ty: Box<TermTy>,
        pat: TermTyPat,
    },
    Infer,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermTyPat {
    pub hir_id: HirId,
    pub kind: TermTyPatKind,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermTyPatKind {
    Range {
        start: Box<TermConstArg>,
        end: Box<TermConstArg>,
    },
    Or {
        pats: Vec<TermTyPat>,
    },
    Err,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermOpaqueTy {
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    // pub generics
    // pub bounds
    // pub origin
    // pub lifetime_mapping,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermBareFnTy {
    pub abi: TermAbi,
    pub generic_params: Vec<TermGenericParam>,
    pub decl: TermFnDecl,
    pub param_idents: Vec<Option<Ident>>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermFnDecl {
    pub inputs: Vec<TermTy>,
    pub output: TermFnRetTy,
    pub c_variadic: bool,
    pub implicit_self: TermImplicitSelfKind,
    pub lifetime_elision_allowed: bool,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermFnRetTy {
    DefaultReturn(Span),
    Return(Box<TermTy>),
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermImplicitSelfKind {
    Imm,
    Mut,
    RefImm,
    RefMut,
    None,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
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
    GpuKernel,
    EfiApi,
    AvrInterrupt,
    AvrNonBlockingInterrupt,
    CCmseNonSecureCall,
    CCmseNonSecureEntry,
    System { unwind: bool },
    RustCall,
    Unadjusted,
    RustCold,
    RiscvInterruptM,
    RiscvInterruptS,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermMutTy {
    pub ty: TermTy,
    pub mutbl: TermMutability,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermAnonConst {
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    pub body: Term,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermMutability {
    Not,
    Mut,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermTraitObjectSyntax {
    Dyn,
    DynStar,
    None,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermTraitRef {
    pub path: TermPath,
    pub hir_ref_id: HirId,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermPath<R = TermRes> {
    pub span: Span,
    pub res: R,
    pub segments: Vec<TermPathSegment>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermGenericParam {
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    pub name: TermParamName,
    pub span: Span,
    pub pure_wrt_drop: bool,
    pub kind: TermGenericParamKind,
    pub colon_span: Option<Span>,
    pub source: TermGenericParamSource,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermParamName {
    Plain(Ident),
    Fresh,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
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
        default: Option<TermConstArg>,
        synthetic: bool,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermLifetimeParamKind {
    Explicit,
    Elided,
    Error,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermGenericParamSource {
    Generics,
    Binder,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermQPath {
    Resolved {
        ty: Option<Box<TermTy>>,
        path: TermPath,
    },
    TypeRelative {
        ty: Box<TermTy>,
        seg: TermPathSegment,
    },
    LangItem {
        item: TermLangItem,
        span: Span,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermPolyTraitRef {
    pub bound_generic_params: Vec<TermGenericParam>,
    pub trait_ref: TermTraitRef,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
pub enum TermRes<Id = HirId> {
    Def {
        def: TermDef,
    },
    PrimTy {
        ty: TermPrimTy,
    },
    SelfTyParam {
        trait_: DefId,
    },
    SelfTyAlias {
        alias_to: DefId,
        forbid_generic: bool,
        is_trait_impl: bool,
    },
    SelfCtor {
        id: DefId,
    },
    Local {
        id: Id,
    },
    ToolMod,
    NonMacroAttr {
        kind: TermNonMacroAttrKind,
    },
    Err,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct TermDef {
    pub kind: TermDefKind,
    pub id: DefId,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermNonMacroAttrKind {
    Builtin(Symbol),
    Tool,
    DeriveHelper,
    DeriveHelperCompat,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "serde_tag")]
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
    Static {
        mutability: TermMutability,
    },
    Ctor {
        ctor_of: TermCtorOf,
        kind: TermCtorKind,
    },
    AssocFn,
    AssocConst,
    Macro {
        kind: TermMacroKind,
    },
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
    Impl {
        of_trait: bool,
    },
    Closure,
    SyntheticCoroutineBody,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermPrimTy {
    Int { ty: TermIntTy },
    Uint { ty: TermUintTy },
    Float { ty: TermFloatTy },
    Str,
    Bool,
    Char,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermIntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermUintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermFloatTy {
    F16,
    F32,
    F64,
    F128,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermCtorOf {
    Struct,
    Variant,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermCtorKind {
    Fn,
    Const,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum TermMacroKind {
    Bang,
    Attr,
    Derive,
}

macro_rules! basic_enum {
    ($name:ident, $p:path, $($vars:ident),*) => {
        #[derive(Debug, Clone, Serialize,  PartialEq, Eq)]
        pub enum $name {
            $($vars),*
        }

        impl<'hir> From<&'hir $p> for $name {
            fn from(value: &'hir $p) -> Self {
                use $p::*;
                match value {
                    $(
                        $vars => Self::$vars
                    ),*
                }
            }
        }
    };
}

basic_enum!(
    TermLangItem,
    rustc_hir::lang_items::LangItem,
    UseCloned,
    UnsafeUnpin,
    UnsafePinned,
    ReceiverTarget,
    LegacyReceiver,
    PanicNullPointerDereference,
    PanicCoroutineResumedDrop,
    PanicAsyncFnResumedDrop,
    PanicAsyncGenFnResumedDrop,
    BikeshedGuaranteedNoDrop,
    RangeCopy,
    RangeInclusiveCopy,
    ContractBuildCheckEnsures,
    ContractCheckRequires,
    DefaultTrait4,
    DefaultTrait3,
    DefaultTrait2,
    DefaultTrait1,
    ContractCheckEnsures,
    RangeMin,
    RangeSub,
    RangeFromCopy,
    CoercePointeeValidated,
    RangeMax,
    PanicGenFnNoneDrop,
    Sized,
    Unsize,
    StructuralPeq,
    Copy,
    Clone,
    CloneFn,
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
    VaList,
    Deref,
    DerefMut,
    DerefPure,
    DerefTarget,
    Receiver,
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
    String,
    CStr
);
