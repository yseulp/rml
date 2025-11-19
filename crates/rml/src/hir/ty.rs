use std::{collections::HashMap, num::NonZero};

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum Ty {
    Bool,
    Char,
    Int {
        ty: IntTy,
    },
    Uint {
        ty: UintTy,
    },
    Float {
        ty: FloatTy,
    },
    Adt {
        def: AdtDef,
        args: Vec<GenericTyArgKind>,
    },
    Foreign {
        def_id: DefId,
    },
    Str,
    Array {
        ty: Box<Ty>,
        len: Box<Const>,
    },
    Pat {
        ty: Box<Ty>,
        pat: Box<Pattern>,
    },
    Slice {
        ty: Box<Ty>,
    },
    RawPtr {
        ty: Box<Ty>,
        r#mut: bool,
    },
    Ref {
        ty: Box<Ty>,
        r#mut: bool,
    },
    FnDef {
        def_id: DefId,
        args: Vec<GenericTyArgKind>,
    },
    FnPtr {
        binder: Binder<FnSigTys>,
        header: FnHeader,
    },
    Dynamic {
        binders: Vec<Binder<ExistentialPredicate>>,
        kind: DynKind,
    },
    Closure {
        def_id: DefId,
        args: Vec<GenericTyArgKind>,
    },
    CoroutineClosure {
        // def_id: DefId,
        // args: Vec<GenericTyArgKind>,
    },
    Coroutine {
        //  def_id: DefId,
        // args: Vec<GenericTyArgKind>,
    },
    CoroutineWitness {
        //  def_id: DefId,
        // args: Vec<GenericTyArgKind>,
    },
    Never,
    Tuple {
        tys: Vec<Ty>,
    },
    Alias {
        kind: AliasTyKind,
        ty: AliasTy,
    },
    Param {
        ty: ParamTy,
    },
    Bound {
        idx: DebruijnIndex,
        ty: BoundTy,
    },
    Placeholder {
        placeholder: Placeholder<BoundTy>,
    },
    Infer {
        // infer: InferTy,
    },
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AdtDef {
    pub did: DefId,
    pub variants: HashMap<VariantIdx, VariantDef>,
    pub flags: AdtFlags,
    // pub repr: ReprOptions,
    pub kind: AdtKind,
    pub path_str: String,
    pub foreign_generics: Option<TyGenerics>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AdtFlags(pub u16);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum AdtKind {
    Struct,
    Union,
    Enum,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Hash)]
pub struct VariantIdx(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct VariantDef {
    pub def_id: DefId,
    pub ctor: Option<(CtorKind, DefId)>,
    pub name: Symbol,
    pub discr: VariantDiscr,
    pub fields: HashMap<FieldIdx, TyFieldDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Hash)]
pub struct FieldIdx(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TyFieldDef {
    pub did: DefId,
    pub name: Symbol,
    pub inst_ty: Ty,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum CtorKind {
    Fn,
    Const,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum VariantDiscr {
    Explicit { def_id: DefId },
    Relative { idx: u32 },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum Const {
    Param {
        pc: ParamConst,
    },
    Infer, //(InferConst),
    Bound {
        idx: DebruijnIndex,
        bound_var: BoundVar,
    },
    Placeholder, //(Placeholder<BoundVar>),
    Unevaluated {
        uc: UnevaluatedConst,
    },
    Value {
        value: Value,
    },
    Error,
    Expr {
        expr: ConstExpr,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Value {
    pub ty: Ty,
    pub valtree: ValTree,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParamConst {
    pub index: u32,
    pub name: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum InferConst {
    Var { id: ConstVid },
    EffectVar { id: EffectVid },
    Fresh { idx: u32 },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ConstVid(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ConstExpr {
    pub kind: ConstExprKind,
    pub args: Vec<GenericTyArgKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum ConstExprKind {
    Binop { kind: BinOpKind },
    UnOp { op: UnOp },
    FunctionCall,
    Cast { kind: CastKind },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum CastKind {
    As,
    Use,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EffectVid(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BoundVar(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnevaluatedConst {
    pub def: DefId,
    pub args: Vec<GenericTyArgKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum ValTree {
    Leaf { scalar_int: ScalarInt },
    Branch { branches: Vec<ValTree> },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ScalarInt {
    pub data: u128,
    pub size: NonZero<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum Pattern {
    Range { start: Const, end: Const },
    Or { pats: Vec<Pattern> },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Binder<T> {
    pub value: T,
    pub bound_vars: Vec<BoundVarKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum BoundVarKind {
    Ty { kind: BoundTyKind },
    Region { region: BoundRegionKind },
    Const,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum BoundTyKind {
    Anon,
    Param { def_id: DefId, symbol: Symbol },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum BoundRegionKind {
    Anon,
    Named { def_id: DefId, symbol: Symbol },
    ClosureEnv,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FnSigTys {
    pub inputs_and_output: Vec<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum ExistentialPredicate {
    Trait { pred: ExistentialTraitRef },
    Projection { pred: ExistentialProjection },
    AutoTrait { def_id: DefId },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ExistentialTraitRef {
    pub def_id: DefId,
    pub args: Vec<GenericTyArgKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ExistentialProjection {
    pub def_id: DefId,
    pub args: Vec<GenericTyArgKind>,
    pub term: TyTerm,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum TyTerm {
    Ty { ty: Ty },
    Const { c: Const },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum DynKind {
    Dyn,
    DynStar,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum AliasTyKind {
    Projection,
    Inherent,
    Opaque,
    Free,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AliasTy {
    pub args: Vec<GenericTyArgKind>,
    pub def_id: DefId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum GenericTyArgKind {
    Lifetime, //(Region)
    Type { ty: Ty },
    Const { r#const: Const },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParamTy {
    pub index: u32,
    pub name: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DebruijnIndex(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BoundTy {
    pub var: BoundVar,
    pub kind: BoundTyKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Placeholder<T> {
    pub universe: UniverseIndex,
    pub bound: T,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UniverseIndex(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum InferTy {
    TyVar { id: TyVid },
    IntVar { id: IntVid },
    FloatVar { id: FloatVid },
    FreshTy { id: u32 },
    FreshIntTy { id: u32 },
    FreshFloatTy { id: u32 },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TyVid(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IntVid(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FloatVid(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TyGenerics {
    pub parent: Option<DefId>,
    pub parent_count: usize,
    pub params: Vec<TyGenericParamDef>,
    pub has_self: bool,
    pub has_self_bound_regions: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TyGenericParamDef {
    pub name: Symbol,
    pub def_id: DefId,
    pub index: u32,
    pub pure_wrt_drop: bool,
    pub kind: TyGenericParamDefKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "serde_tag")]
pub enum TyGenericParamDefKind {
    Lifetime,
    Type { has_default: bool, synthetic: bool },
    Const { has_default: bool, synthetic: bool },
}
