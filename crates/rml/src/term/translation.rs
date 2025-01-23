//! Handle translation of HIR structures to [Term], and the requires
//! sub-structures.
//!
//! See especially: [FromHir], [HirInto].

use rustc_ast::{
    BindingMode, BorrowKind, ByRef, CaptureBy, FloatTy, IntTy, LitFloatType, LitIntType, LitKind,
    Mutability, StrStyle, TraitObjectSyntax, UintTy,
};
use rustc_hir::{
    def::{CtorKind, CtorOf, DefKind, NonMacroAttrKind, Res},
    AnonConst, Arm, ArrayLen, BareFnTy, BinOp, BinOpKind, Block, BlockCheckMode, Body, Closure,
    ClosureBinder, ConstArg, ConstArgKind, Constness, DotDotPos, Expr, ExprField, ExprKind, FnDecl,
    FnRetTy, GenericArg, GenericArgs, GenericArgsParentheses, GenericBound, GenericParam,
    GenericParamKind, GenericParamSource, ImplicitSelfKind, InferArg, LetExpr, LetStmt, Lifetime,
    LifetimeName, LifetimeParamKind, Lit, MatchSource, MutTy, OpaqueTy, Param, ParamName, Pat,
    PatField, PatKind, Path, PathSegment, PolyTraitRef, PrimTy, QPath, RangeEnd, Stmt, StmtKind,
    TraitRef, Ty, TyKind, UnOp, UnsafeSource,
};
use rustc_middle::hir::map::Map;
use rustc_span::{MacroKind, Symbol};
use rustc_target::spec::abi::Abi;

use super::{
    HirTerm, QuantorKind, QuantorParam, Term, TermAbi, TermAnonConst, TermArm, TermArrayLen,
    TermBareFnTy, TermBinOp, TermBinOpKind, TermBindingMode, TermBlock, TermBlockCheckMode,
    TermBody, TermBorrowKind, TermByRef, TermCaptureBy, TermClosure, TermClosureBinder,
    TermConstArg, TermConstArgKind, TermConstness, TermCtorKind, TermCtorOf, TermDefKind,
    TermDotDotPos, TermField, TermFloatTy, TermFnDecl, TermFnRetTy, TermGenericArg,
    TermGenericArgs, TermGenericArgsParentheses, TermGenericBound, TermGenericParam,
    TermGenericParamKind, TermGenericParamSource, TermImplicitSelfKind, TermInferArg, TermIntTy,
    TermKind, TermLet, TermLetStmt, TermLifetime, TermLifetimeName, TermLifetimeParamKind, TermLit,
    TermLitFloatType, TermLitIntType, TermLitKind, TermMacroKind, TermMatchSource, TermMutTy,
    TermMutability, TermNonMacroAttrKind, TermOpaqueTy, TermParam, TermParamName, TermPat,
    TermPatField, TermPatKind, TermPath, TermPathSegment, TermPolyTraitRef, TermPrimTy, TermQPath,
    TermRangeEnd, TermRes, TermStmt, TermStmtKind, TermStrStyle, TermTraitObjectSyntax,
    TermTraitRef, TermTy, TermTyKind, TermUintTy, TermUnOp, TermUnsafeSource,
};

/// Allows translating from `T` to `Self`, where `T` is a HIR structure. Since
/// some structures reference bodies, we require access to the HIR.
pub trait FromHir<'hir, T>
where
    T: Sized,
{
    /// Translate from `value` to `Self`, where `T` is a HIR structure. Since
    /// some structures reference bodies, we require access to the HIR via
    /// `hir`.
    fn from_hir(value: T, hir: Map<'hir>) -> Self;
}

/// Allows translating from `Self` to `T`, where `Self` is a HIR structure.
/// Since some structures reference bodies, we require access to the HIR.
///
/// **Do not implement this directly.** Use [FromHir] instead.
pub trait HirInto<'hir, T>
where
    T: Sized,
{
    /// Translate from `self` to `T`, where `self` is a HIR structure. Since
    /// some structures reference bodies, we require access to the HIR via
    /// `hir`.
    fn hir_into(self, hir: Map<'hir>) -> T;
}

impl<'hir, T, U> HirInto<'hir, U> for T
where
    U: FromHir<'hir, T>,
{
    fn hir_into(self, hir: Map<'hir>) -> U {
        U::from_hir(self, hir)
    }
}

impl<'hir> FromHir<'hir, &'hir Expr<'hir>> for Term {
    fn from_hir(value: &'hir Expr<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            kind: value.kind.hir_into(hir),
        }
    }
}

impl<'hir> FromHir<'hir, ExprKind<'hir>> for TermKind {
    fn from_hir(value: ExprKind<'hir>, hir: Map<'hir>) -> Self {
        match value {
            ExprKind::Array(exprs) => Self::Array {
                terms: exprs.iter().map(|e| e.hir_into(hir)).collect(),
            },
            ExprKind::Call(recv, args) => match recv.kind {
                ExprKind::Path(QPath::Resolved(None, Path { segments, .. }))
                    if is_rml_fn(segments) =>
                {
                    let kind = get_rml_fn_kind(segments).expect("expected valid rml function");

                    if let Some(qkind) = kind.get_quant_kind() {
                        let clo: TermClosure = match args[0].kind {
                            ExprKind::Closure(c) => c.hir_into(hir),
                            _ => unreachable!(),
                        };
                        let param = {
                            let c_param = &clo.body.params[0];

                            let ident = match &c_param.pat.kind {
                                TermPatKind::Binding(_, _, ident, _) => ident.clone(),
                                _ => unreachable!(),
                            };

                            QuantorParam {
                                hir_id: c_param.hir_id,
                                ident,
                                ty: clo.fn_decl.inputs[0].clone(),
                                span: c_param.span,
                                ty_span: c_param.ty_span,
                            }
                        };

                        let term = clo.body.value;

                        return Self::Quantor {
                            kind: qkind,
                            param,
                            term,
                        };
                    }

                    match kind {
                        RmlFnKind::Stub(StubKind::Equiv) => {
                            let left: Term = (&args[0]).hir_into(hir);
                            let right: Term = (&args[1]).hir_into(hir);

                            TermKind::Binary {
                                op: TermBinOp {
                                    node: TermBinOpKind::LogEq,
                                    span: recv.span.into(),
                                },
                                left: left.into(),
                                right: right.into(),
                            }
                        }
                        RmlFnKind::Stub(_) => unreachable!(),
                        RmlFnKind::TraitFn(TraitFn::ShallowModel) => {
                            let term: Term = (&args[0]).hir_into(hir);
                            TermKind::Model {
                                kind: super::ModelKind::Shallow,
                                term: term.into(),
                            }
                        }
                        RmlFnKind::TraitFn(TraitFn::DeepModel) => {
                            let term: Term = (&args[0]).hir_into(hir);
                            TermKind::Model {
                                kind: super::ModelKind::Deep,
                                term: term.into(),
                            }
                        }
                        RmlFnKind::TraitFn(TraitFn::Index) => {
                            let term: Term = (&args[0]).hir_into(hir);
                            let index: Term = (&args[1]).hir_into(hir);
                            let span = index.span;

                            TermKind::Index {
                                term: term.into(),
                                idx: index.into(),
                                span,
                            }
                        }
                        RmlFnKind::TraitFn(
                            ord @ TraitFn::OrdLt
                            | ord @ TraitFn::OrdLe
                            | ord @ TraitFn::OrdGe
                            | ord @ TraitFn::OrdGt,
                        ) => {
                            let left: Term = (&args[0]).hir_into(hir);
                            let right: Term = (&args[1]).hir_into(hir);

                            let node = match ord {
                                TraitFn::OrdLt => TermBinOpKind::Lt,
                                TraitFn::OrdLe => TermBinOpKind::Le,
                                TraitFn::OrdGe => TermBinOpKind::Ge,
                                TraitFn::OrdGt => TermBinOpKind::Gt,
                                _ => unreachable!(),
                            };

                            TermKind::Binary {
                                op: TermBinOp {
                                    node,
                                    span: recv.span.into(),
                                },
                                left: left.into(),
                                right: right.into(),
                            }
                        }
                    }
                }
                _ => Self::Call {
                    callee: Box::new(recv.hir_into(hir)),
                    args: args.iter().map(|a| a.hir_into(hir)).collect(),
                },
            },
            ExprKind::MethodCall(path, recv, args, span) => Self::MethodCall {
                path: path.hir_into(hir),
                callee: Box::new(recv.hir_into(hir)),
                args: args.iter().map(|a| a.hir_into(hir)).collect(),
                span: span.into(),
            },
            ExprKind::Tup(exprs) => Self::Tup {
                terms: exprs.iter().map(|e| e.hir_into(hir)).collect(),
            },
            ExprKind::Binary(op, left, right) => Self::Binary {
                op: op.into(),
                left: Box::new(left.hir_into(hir)),
                right: Box::new(right.hir_into(hir)),
            },
            ExprKind::Unary(op, expr) => Self::Unary {
                op: op.into(),
                child: Box::new(expr.hir_into(hir)),
            },
            ExprKind::Lit(lit) => Self::Lit { lit: lit.into() },
            ExprKind::Cast(expr, ty) => Self::Cast {
                term: Box::new(expr.hir_into(hir)),
                ty: ty.hir_into(hir),
            },
            ExprKind::Let(l) => Self::Let {
                r#let: l.hir_into(hir),
            },
            ExprKind::If(cond, then, els) => Self::If {
                cond: Box::new(cond.hir_into(hir)),
                then: Box::new(then.hir_into(hir)),
                r#else: els.map(|e| Box::new(e.hir_into(hir))),
            },
            ExprKind::Match(expr, arms, source) => Self::Match {
                term: Box::new(expr.hir_into(hir)),
                arms: arms.iter().map(|a| a.hir_into(hir)).collect(),
                src: source.into(),
            },
            ExprKind::Closure(c) => Self::Closure {
                closure: c.hir_into(hir),
            },
            ExprKind::Block(b, _) => Self::Block {
                block: b.hir_into(hir),
            },
            ExprKind::Field(expr, field) => Self::Field {
                term: Box::new(expr.hir_into(hir)),
                field: field.into(),
            },
            ExprKind::Index(expr, idx, span) => Self::Index {
                term: Box::new(expr.hir_into(hir)),
                idx: Box::new(idx.hir_into(hir)),
                span: span.into(),
            },
            ExprKind::Path(p) => Self::Path {
                path: p.hir_into(hir),
            },
            ExprKind::AddrOf(brw, m, expr) => Self::AddrOf {
                kind: brw.into(),
                mutability: m.into(),
                term: Box::new(expr.hir_into(hir)),
            },
            ExprKind::Struct(p, fields, rest) => Self::Struct {
                path: p.hir_into(hir),
                fields: fields.iter().map(|f| f.hir_into(hir)).collect(),
                rest: rest.map(|r| Box::new(r.hir_into(hir))),
            },
            ExprKind::Repeat(expr, len) => Self::Repeat {
                term: Box::new(expr.hir_into(hir)),
                len: Box::new(len.hir_into(hir)),
            },
            k => panic!("Unsupported kind {k:?}"),
        }
    }
}

impl From<BorrowKind> for TermBorrowKind {
    fn from(value: BorrowKind) -> Self {
        match value {
            BorrowKind::Ref => Self::Ref,
            BorrowKind::Raw => Self::Raw,
        }
    }
}

impl<'hir> FromHir<'hir, ArrayLen<'hir>> for TermArrayLen {
    fn from_hir(value: ArrayLen<'hir>, hir: Map<'hir>) -> Self {
        match value {
            ArrayLen::Infer(a) => Self::Infer((&a).into()),
            ArrayLen::Body(c) => Self::Body(c.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir LetExpr<'hir>> for TermLet {
    fn from_hir(value: &'hir LetExpr<'hir>, hir: Map<'hir>) -> Self {
        Self {
            span: value.span.into(),
            pat: value.pat.hir_into(hir),
            ty: value.ty.map(|t| t.hir_into(hir)),
            init: Box::new(value.init.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir LetStmt<'hir>> for TermLetStmt {
    fn from_hir(value: &'hir LetStmt<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            pat: value.pat.hir_into(hir),
            ty: value.ty.map(|ty| ty.hir_into(hir)),
            init: Box::new(value.init.unwrap().hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Arm<'hir>> for TermArm {
    fn from_hir(value: &'hir Arm<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            pat: value.pat.hir_into(hir),
            guard: value.guard.map(|g| g.hir_into(hir)),
            body: Box::new(value.body.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Closure<'hir>> for TermClosure {
    fn from_hir(value: &'hir Closure<'hir>, hir: Map<'hir>) -> Self {
        Self {
            def_id: value.def_id.into(),
            binder: value.binder.into(),
            constness: value.constness.into(),
            capture_clause: value.capture_clause.into(),
            bound_generic_params: value
                .bound_generic_params
                .iter()
                .map(|p| p.hir_into(hir))
                .collect(),
            fn_decl: value.fn_decl.hir_into(hir),
            body: hir.body(value.body).hir_into(hir),
            fn_decl_span: value.fn_decl_span.into(),
            fn_arg_span: value.fn_arg_span.map(|s| s.into()),
        }
    }
}

impl From<ClosureBinder> for TermClosureBinder {
    fn from(value: ClosureBinder) -> Self {
        match value {
            ClosureBinder::Default => Self::Default,
            ClosureBinder::For { span } => Self::For { span: span.into() },
        }
    }
}

impl From<Constness> for TermConstness {
    fn from(value: Constness) -> Self {
        match value {
            Constness::Const => Self::Const,
            Constness::NotConst => Self::NotConst,
        }
    }
}

impl From<CaptureBy> for TermCaptureBy {
    fn from(value: CaptureBy) -> Self {
        match value {
            CaptureBy::Value { move_kw } => Self::Value {
                move_kw: move_kw.into(),
            },
            CaptureBy::Ref => Self::Ref,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir FnDecl<'hir>> for TermFnDecl {
    fn from_hir(value: &'hir FnDecl<'hir>, hir: Map<'hir>) -> Self {
        Self {
            inputs: value.inputs.iter().map(|i| i.hir_into(hir)).collect(),
            output: value.output.hir_into(hir),
            c_variadic: value.c_variadic,
            implicit_self: value.implicit_self.into(),
            lifetime_elision_allowed: value.lifetime_elision_allowed,
        }
    }
}

impl<'hir> FromHir<'hir, FnRetTy<'hir>> for TermFnRetTy {
    fn from_hir(value: FnRetTy<'hir>, hir: Map<'hir>) -> Self {
        match value {
            FnRetTy::DefaultReturn(sp) => Self::DefaultReturn(sp.into()),
            FnRetTy::Return(ty) => Self::Return(Box::new(ty.hir_into(hir))),
        }
    }
}

impl From<ImplicitSelfKind> for TermImplicitSelfKind {
    fn from(value: ImplicitSelfKind) -> Self {
        match value {
            ImplicitSelfKind::Imm => Self::Imm,
            ImplicitSelfKind::Mut => Self::Mut,
            ImplicitSelfKind::RefImm => Self::RefImm,
            ImplicitSelfKind::RefMut => Self::RefMut,
            ImplicitSelfKind::None => Self::None,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Block<'hir>> for TermBlock {
    fn from_hir(value: &'hir Block<'hir>, hir: Map<'hir>) -> Self {
        Self {
            stmts: value.stmts.iter().map(|s| s.hir_into(hir)).collect(),
            term: value.expr.map(|e| Box::new(e.hir_into(hir))),
            hir_id: value.hir_id.into(),
            rules: value.rules.into(),
            span: value.span.into(),
        }
    }
}

impl From<BlockCheckMode> for TermBlockCheckMode {
    fn from(value: BlockCheckMode) -> Self {
        match value {
            BlockCheckMode::DefaultBlock => Self::DefaultBlock,
            BlockCheckMode::UnsafeBlock(s) => Self::UnsafeBlock(s.into()),
        }
    }
}

impl From<UnsafeSource> for TermUnsafeSource {
    fn from(value: UnsafeSource) -> Self {
        match value {
            UnsafeSource::CompilerGenerated => Self::CompilerGenerated,
            UnsafeSource::UserProvided => Self::UserProvided,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir ExprField<'hir>> for TermField {
    fn from_hir(value: &'hir ExprField<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            term: value.expr.hir_into(hir),
            span: value.span.into(),
            is_shorthand: value.is_shorthand,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Stmt<'hir>> for TermStmt {
    fn from_hir(value: &'hir Stmt<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: value.kind.hir_into(hir),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, StmtKind<'hir>> for TermStmtKind {
    fn from_hir(value: StmtKind<'hir>, hir: Map<'hir>) -> Self {
        match value {
            StmtKind::Let(l) => Self::Let(l.hir_into(hir)),
            StmtKind::Item(i) => Self::Item(i.into()),
            StmtKind::Expr(e) => Self::Term(Box::new(e.hir_into(hir))),
            StmtKind::Semi(e) => Self::Semi(Box::new(e.hir_into(hir))),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Body<'hir>> for TermBody {
    fn from_hir(value: &'hir Body<'hir>, hir: Map<'hir>) -> Self {
        Self {
            params: value.params.iter().map(|p| p.hir_into(hir)).collect(),
            value: Box::new(value.value.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Param<'hir>> for TermParam {
    fn from_hir(value: &'hir Param, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            pat: value.pat.hir_into(hir),
            ty_span: value.ty_span.into(),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Pat<'hir>> for TermPat {
    fn from_hir(value: &'hir Pat<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: value.kind.hir_into(hir),
            span: value.span.into(),
            default_binding_modes: value.default_binding_modes,
        }
    }
}

impl<'hir> FromHir<'hir, PatKind<'hir>> for TermPatKind {
    fn from_hir(value: PatKind<'hir>, hir: Map<'hir>) -> Self {
        match value {
            PatKind::Wild => Self::Wild,
            PatKind::Binding(ann, hid, ident, pat) => Self::Binding(
                ann.into(),
                hid.into(),
                ident.into(),
                pat.map(|p| Box::new(p.hir_into(hir))),
            ),
            PatKind::Struct(qp, fields, rest) => Self::Struct(
                qp.hir_into(hir),
                fields.iter().map(|f| f.hir_into(hir)).collect(),
                rest,
            ),
            PatKind::TupleStruct(qp, elems, ddp) => Self::TupleStruct(
                qp.hir_into(hir),
                elems.iter().map(|e| e.hir_into(hir)).collect(),
                ddp.into(),
            ),
            PatKind::Or(pats) => Self::Or(pats.iter().map(|p| p.hir_into(hir)).collect()),
            PatKind::Path(qp) => Self::Path(qp.hir_into(hir)),
            PatKind::Tuple(pats, ddp) => {
                Self::Tuple(pats.iter().map(|p| p.hir_into(hir)).collect(), ddp.into())
            }
            PatKind::Box(p) => Self::Box(Box::new(p.hir_into(hir))),
            PatKind::Ref(p, m) => Self::Ref(Box::new(p.hir_into(hir)), m.into()),
            PatKind::Lit(e) => Self::Lit(Box::new(e.hir_into(hir))),
            PatKind::Range(from, to, re) => Self::Range(
                from.map(|f| Box::new(f.hir_into(hir))),
                to.map(|t| Box::new(t.hir_into(hir))),
                re.into(),
            ),
            PatKind::Slice(start, mid, end) => Self::Slice(
                start.iter().map(|p| p.hir_into(hir)).collect(),
                mid.map(|m| Box::new(m.hir_into(hir))),
                end.iter().map(|p| p.hir_into(hir)).collect(),
            ),
            PatKind::Never => Self::Never,
            PatKind::Deref(pat) => Self::Deref(Box::new(pat.hir_into(hir))),
            PatKind::Err(_) => Self::Err,
        }
    }
}

impl From<BindingMode> for TermBindingMode {
    fn from(value: BindingMode) -> Self {
        Self(value.0.into(), value.1.into())
    }
}

impl<'hir> FromHir<'hir, &'hir PatField<'hir>> for TermPatField {
    fn from_hir(value: &'hir PatField<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            pat: value.pat.hir_into(hir),
            is_shorthand: value.is_shorthand,
            span: value.span.into(),
        }
    }
}

impl From<DotDotPos> for TermDotDotPos {
    fn from(value: DotDotPos) -> Self {
        Self(value.as_opt_usize().map(|u| u as u32).unwrap_or(u32::MAX))
    }
}

impl From<RangeEnd> for TermRangeEnd {
    fn from(value: RangeEnd) -> Self {
        match value {
            RangeEnd::Included => Self::Included,
            RangeEnd::Excluded => Self::Excluded,
        }
    }
}

impl From<ByRef> for TermByRef {
    fn from(value: ByRef) -> Self {
        match value {
            ByRef::Yes(Mutability::Mut) => Self::Yes { mutable: true },
            ByRef::Yes(Mutability::Not) => Self::Yes { mutable: false },
            ByRef::No => Self::No,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir PathSegment<'hir>> for TermPathSegment {
    fn from_hir(value: &PathSegment<'hir>, hir: Map<'hir>) -> Self {
        Self {
            ident: value.ident.into(),
            hir_id: value.hir_id.into(),
            res: value.res.into(),
            args: value.args.map(|a| a.hir_into(hir)),
            infer_args: value.infer_args,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Ty<'hir>> for TermTy {
    fn from_hir(value: &'hir Ty<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: value.kind.hir_into(hir),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, TyKind<'hir>> for TermTyKind {
    fn from_hir(value: TyKind<'hir>, hir: Map<'hir>) -> Self {
        match value {
            TyKind::Slice(ty) => Self::Slice(Box::new(ty.hir_into(hir))),
            TyKind::Array(ty, len) => {
                Self::Array(Box::new(ty.hir_into(hir)), Box::new(len.hir_into(hir)))
            }
            TyKind::Ptr(mty) => Self::Ptr(Box::new(mty.hir_into(hir))),
            TyKind::Ref(l, mty) => Self::Ref(l.into(), Box::new(mty.hir_into(hir))),
            TyKind::BareFn(f) => Self::BareFn(f.hir_into(hir)),
            TyKind::Never => Self::Never,
            TyKind::Tup(tys) => Self::Tup(tys.iter().map(|ty| ty.hir_into(hir)).collect()),
            TyKind::Path(p) => Self::Path(p.hir_into(hir)),
            TyKind::OpaqueDef(ty, args) => Self::OpaqueDef(
                ty.hir_into(hir),
                args.iter().map(|a| a.hir_into(hir)).collect(),
            ),
            TyKind::TraitObject(prefs, l, syn) => Self::TraitObject(
                prefs.iter().map(|r| r.hir_into(hir)).collect(),
                l.into(),
                syn.into(),
            ),
            TyKind::Typeof(c) => Self::Typeof(Box::new(c.hir_into(hir))),
            TyKind::Infer => Self::Infer,
            TyKind::InferDelegation(did, _) => Self::InferDelegation(did.into()),
            TyKind::AnonAdt(id) => Self::AnonAdt(id.into()),
            TyKind::Pat(ty, pat) => Self::Pat(Box::new(ty.hir_into(hir)), pat.hir_into(hir)),
            TyKind::Err(_) => unreachable!(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir OpaqueTy<'hir>> for TermOpaqueTy {
    fn from_hir(value: &'hir OpaqueTy<'hir>, _hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, MutTy<'hir>> for TermMutTy {
    fn from_hir(value: MutTy<'hir>, hir: Map<'hir>) -> Self {
        Self {
            ty: value.ty.hir_into(hir),
            mutbl: value.mutbl.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir BareFnTy<'hir>> for TermBareFnTy {
    fn from_hir(value: &'hir BareFnTy<'hir>, hir: Map<'hir>) -> Self {
        Self {
            abi: value.abi.into(),
            generic_params: value
                .generic_params
                .iter()
                .map(|p| p.hir_into(hir))
                .collect(),
            decl: value.decl.hir_into(hir),
            param_names: value
                .param_names
                .iter()
                .map(|ident| (*ident).into())
                .collect(),
        }
    }
}

impl From<Abi> for TermAbi {
    fn from(value: Abi) -> Self {
        match value {
            Abi::Rust => Self::Rust,
            Abi::C { unwind } => Self::C { unwind },
            Abi::Cdecl { unwind } => Self::Cdecl { unwind },
            Abi::Stdcall { unwind } => Self::Stdcall { unwind },
            Abi::Fastcall { unwind } => Self::Fastcall { unwind },
            Abi::Vectorcall { unwind } => Self::Vectorcall { unwind },
            Abi::Thiscall { unwind } => Self::Thiscall { unwind },
            Abi::Aapcs { unwind } => Self::Aapcs { unwind },
            Abi::Win64 { unwind } => Self::Win64 { unwind },
            Abi::SysV64 { unwind } => Self::SysV64 { unwind },
            Abi::PtxKernel => Self::PtxKernel,
            Abi::Msp430Interrupt => Self::Msp430Interrupt,
            Abi::X86Interrupt => Self::X86Interrupt,
            Abi::EfiApi => Self::EfiApi,
            Abi::AvrInterrupt => Self::AvrNonBlockingInterrupt,
            Abi::AvrNonBlockingInterrupt => Self::AvrNonBlockingInterrupt,
            Abi::CCmseNonSecureCall => Self::CCmseNonSecureCall,
            Abi::System { unwind } => Self::System { unwind },
            Abi::RustIntrinsic => Self::RustIntrinsic,
            Abi::RustCall => Self::RustCall,
            Abi::Unadjusted => Self::Unadjusted,
            Abi::RustCold => Self::RustCold,
            Abi::RiscvInterruptM => Self::RiscvInterruptM,
            Abi::RiscvInterruptS => Self::RiscvInterruptS,
            _ => todo!("Abi"),
        }
    }
}

impl From<TraitObjectSyntax> for TermTraitObjectSyntax {
    fn from(value: TraitObjectSyntax) -> Self {
        match value {
            TraitObjectSyntax::Dyn => Self::Dyn,
            TraitObjectSyntax::DynStar => Self::DynStar,
            TraitObjectSyntax::None => Self::None,
        }
    }
}

impl From<BinOp> for TermBinOp {
    fn from(value: BinOp) -> Self {
        Self {
            node: value.node.into(),
            span: value.span.into(),
        }
    }
}

impl From<BinOpKind> for TermBinOpKind {
    fn from(value: BinOpKind) -> Self {
        match value {
            rustc_hir::BinOpKind::Add => TermBinOpKind::Add,
            rustc_hir::BinOpKind::Sub => TermBinOpKind::Sub,
            rustc_hir::BinOpKind::Mul => TermBinOpKind::Mul,
            rustc_hir::BinOpKind::Div => TermBinOpKind::Div,
            rustc_hir::BinOpKind::Rem => TermBinOpKind::Rem,
            rustc_hir::BinOpKind::And => TermBinOpKind::And,
            rustc_hir::BinOpKind::Or => TermBinOpKind::Or,
            rustc_hir::BinOpKind::BitXor => TermBinOpKind::BitXor,
            rustc_hir::BinOpKind::BitAnd => TermBinOpKind::BitAnd,
            rustc_hir::BinOpKind::BitOr => TermBinOpKind::BitOr,
            rustc_hir::BinOpKind::Shl => TermBinOpKind::Shl,
            rustc_hir::BinOpKind::Shr => TermBinOpKind::Shr,
            rustc_hir::BinOpKind::Eq => TermBinOpKind::Eq,
            rustc_hir::BinOpKind::Lt => TermBinOpKind::Lt,
            rustc_hir::BinOpKind::Le => TermBinOpKind::Le,
            rustc_hir::BinOpKind::Ne => TermBinOpKind::Ne,
            rustc_hir::BinOpKind::Ge => TermBinOpKind::Ge,
            rustc_hir::BinOpKind::Gt => TermBinOpKind::Gt,
        }
    }
}

impl From<UnOp> for TermUnOp {
    fn from(value: UnOp) -> Self {
        match value {
            UnOp::Deref => Self::Deref,
            UnOp::Not => Self::Not,
            UnOp::Neg => Self::Neg,
        }
    }
}

impl<'hir> From<&'hir Lit> for TermLit {
    fn from(value: &'hir Lit) -> Self {
        Self {
            node: (&value.node).into(),
            span: value.span.into(),
        }
    }
}

impl<'hir> From<&'hir LitKind> for TermLitKind {
    fn from(value: &'hir LitKind) -> Self {
        match value {
            LitKind::Str(sym, style) => Self::Str {
                symbol: (*sym).into(),
                style: style.into(),
            },
            LitKind::ByteStr(sl, style) => Self::ByteStr {
                bytes: sl.clone(),
                style: style.into(),
            },
            LitKind::CStr(sl, style) => Self::CStr {
                bytes: sl.clone(),
                style: style.into(),
            },
            LitKind::Byte(b) => Self::Byte { value: *b },
            LitKind::Char(c) => Self::Char { value: *c },
            LitKind::Int(i, t) => Self::Int {
                value: i.0,
                ty: t.into(),
            },
            LitKind::Float(sym, t) => Self::Float {
                symbol: (*sym).into(),
                ty: t.into(),
            },
            LitKind::Bool(b) => Self::Bool { value: *b },
            LitKind::Err(_) => unreachable!(),
        }
    }
}

impl<'hir> From<&'hir StrStyle> for TermStrStyle {
    fn from(value: &'hir StrStyle) -> Self {
        match value {
            StrStyle::Cooked => Self::Cooked,
            StrStyle::Raw(r) => Self::Raw { number: *r },
        }
    }
}

impl<'hir> From<&'hir LitIntType> for TermLitIntType {
    fn from(value: &'hir LitIntType) -> Self {
        match value {
            LitIntType::Signed(s) => Self::Signed { ty: (*s).into() },
            LitIntType::Unsigned(u) => Self::Unsigned { ty: (*u).into() },
            LitIntType::Unsuffixed => Self::Unsuffixed,
        }
    }
}

impl<'hir> From<&'hir LitFloatType> for TermLitFloatType {
    fn from(value: &'hir LitFloatType) -> Self {
        match value {
            LitFloatType::Suffixed(s) => Self::Suffixed { ty: (*s).into() },
            LitFloatType::Unsuffixed => Self::Unsuffixed,
        }
    }
}

impl From<MatchSource> for TermMatchSource {
    fn from(value: MatchSource) -> Self {
        match value {
            MatchSource::Normal => Self::Normal,
            MatchSource::Postfix => Self::Postfix,
            MatchSource::ForLoopDesugar => Self::ForLoopDesugar,
            MatchSource::TryDesugar(hir_id) => Self::TryDesugar(hir_id.into()),
            MatchSource::AwaitDesugar => Self::AwaitDesugar,
            MatchSource::FormatArgs => Self::FormatArgs,
        }
    }
}

impl<'hir> FromHir<'hir, QPath<'hir>> for TermQPath {
    fn from_hir(value: QPath<'hir>, hir: Map<'hir>) -> Self {
        match value {
            QPath::Resolved(ty, path) => Self::Resolved {
                ty: ty.map(|ty| Box::new(ty.hir_into(hir))),
                path: path.hir_into(hir),
            },
            QPath::TypeRelative(ty, ps) => Self::TypeRelative {
                ty: Box::new(ty.hir_into(hir)),
                seg: ps.hir_into(hir),
            },
            QPath::LangItem(li, sp) => Self::LangItem {
                item: (&li).into(),
                span: sp.into(),
            },
        }
    }
}

impl<'hir> FromHir<'hir, &'hir QPath<'hir>> for TermQPath {
    fn from_hir(value: &'hir QPath<'hir>, hir: Map<'hir>) -> Self {
        (*value).hir_into(hir)
    }
}

impl<'hir> FromHir<'hir, &'hir GenericArgs<'hir>> for TermGenericArgs {
    fn from_hir(value: &'hir GenericArgs<'hir>, hir: Map<'hir>) -> Self {
        Self {
            args: value.args.iter().map(|a| a.hir_into(hir)).collect(),
            // bindings: value.bindings.iter().map(|b| b.hir_into(hir)).collect(),
            parenthesized: value.parenthesized.into(),
            span_ext: value.span_ext.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir GenericArg<'hir>> for TermGenericArg {
    fn from_hir(value: &'hir GenericArg<'hir>, hir: Map<'hir>) -> Self {
        match value {
            GenericArg::Lifetime(l) => Self::Lifetime((*l).into()),
            GenericArg::Type(ty) => Self::Type((*ty).hir_into(hir)),
            GenericArg::Const(c) => Self::Const((*c).hir_into(hir)),
            GenericArg::Infer(i) => Self::Infer(i.into()),
        }
    }
}

impl<'hir> From<&'hir Lifetime> for TermLifetime {
    fn from(value: &'hir Lifetime) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            res: value.res.into(),
        }
    }
}

impl From<LifetimeName> for TermLifetimeName {
    fn from(value: LifetimeName) -> Self {
        match value {
            LifetimeName::Param(p) => Self::Param(p.into()),
            LifetimeName::ImplicitObjectLifetimeDefault => Self::ImplicitObjectLifetimeDefault,
            LifetimeName::Error => unreachable!(),
            LifetimeName::Infer => Self::Infer,
            LifetimeName::Static => Self::Static,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir ConstArg<'hir>> for TermConstArg {
    fn from_hir(value: &'hir ConstArg<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: (&value.kind).hir_into(hir),
            is_desugared_from_effects: value.is_desugared_from_effects,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir ConstArgKind<'hir>> for TermConstArgKind {
    fn from_hir(value: &'hir ConstArgKind<'hir>, hir: Map<'hir>) -> Self {
        match value {
            ConstArgKind::Path(q) => Self::Path(q.hir_into(hir)),
            ConstArgKind::Anon(a) => Self::Anon((*a).hir_into(hir)),
        }
    }
}

impl<'hir> From<&'hir InferArg> for TermInferArg {
    fn from(value: &'hir InferArg) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
        }
    }
}

impl From<GenericArgsParentheses> for TermGenericArgsParentheses {
    fn from(value: GenericArgsParentheses) -> Self {
        match value {
            GenericArgsParentheses::No => Self::No,
            GenericArgsParentheses::ReturnTypeNotation => Self::ReturnTypeNotation,
            GenericArgsParentheses::ParenSugar => Self::ParenSugar,
        }
    }
}

impl<'hir> FromHir<'hir, rustc_hir::Term<'hir>> for HirTerm {
    fn from_hir(value: rustc_hir::Term<'hir>, hir: Map<'hir>) -> Self {
        match value {
            rustc_hir::Term::Ty(ty) => Self::Ty(ty.hir_into(hir)),
            rustc_hir::Term::Const(c) => Self::Const(c.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir GenericBound<'hir>> for TermGenericBound {
    fn from_hir(value: &'hir GenericBound<'hir>, hir: Map<'hir>) -> Self {
        match value {
            GenericBound::Trait(pr) => Self::Trait(pr.hir_into(hir)),
            GenericBound::Outlives(l) => Self::Outlives((*l).into()),
            GenericBound::Use(..) => todo!("GenericBound::Use"),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir PolyTraitRef<'hir>> for TermPolyTraitRef {
    fn from_hir(value: &'hir PolyTraitRef<'hir>, hir: Map<'hir>) -> Self {
        Self {
            bound_generic_params: value
                .bound_generic_params
                .iter()
                .map(|p| p.hir_into(hir))
                .collect(),
            trait_ref: value.trait_ref.hir_into(hir),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir GenericParam<'hir>> for TermGenericParam {
    fn from_hir(value: &'hir GenericParam<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            name: value.name.into(),
            span: value.span.into(),
            pure_wrt_drop: value.pure_wrt_drop,
            kind: value.kind.hir_into(hir),
            colon_span: value.colon_span.map(|sp| sp.into()),
            source: value.source.into(),
        }
    }
}

impl From<ParamName> for TermParamName {
    fn from(value: ParamName) -> Self {
        match value {
            ParamName::Plain(i) => Self::Plain(i.into()),
            ParamName::Fresh => Self::Fresh,
            ParamName::Error => unreachable!(),
        }
    }
}

impl<'hir> FromHir<'hir, GenericParamKind<'hir>> for TermGenericParamKind {
    fn from_hir(value: GenericParamKind<'hir>, hir: Map<'hir>) -> Self {
        match value {
            GenericParamKind::Lifetime { kind } => Self::Lifetime { kind: kind.into() },
            GenericParamKind::Type { default, synthetic } => Self::Type {
                default: default.map(|ty| ty.hir_into(hir)),
                synthetic,
            },
            GenericParamKind::Const {
                ty,
                default,
                is_host_effect,
                synthetic,
            } => Self::Const {
                ty: ty.hir_into(hir),
                default: default.map(|ac| ac.hir_into(hir)),
                is_host_effect,
                synthetic,
            },
        }
    }
}

impl From<LifetimeParamKind> for TermLifetimeParamKind {
    fn from(value: LifetimeParamKind) -> Self {
        match value {
            LifetimeParamKind::Explicit => Self::Explicit,
            LifetimeParamKind::Elided(..) => Self::Elided,
            LifetimeParamKind::Error => Self::Error,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir AnonConst> for TermAnonConst {
    fn from_hir(value: &'hir AnonConst, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            body: hir.body(value.body).value.hir_into(hir),
        }
    }
}

impl From<GenericParamSource> for TermGenericParamSource {
    fn from(value: GenericParamSource) -> Self {
        match value {
            GenericParamSource::Generics => Self::Generics,
            GenericParamSource::Binder => Self::Binder,
        }
    }
}

impl<'hir> FromHir<'hir, TraitRef<'hir>> for TermTraitRef {
    fn from_hir(value: TraitRef<'hir>, hir: Map<'hir>) -> Self {
        Self {
            path: value.path.hir_into(hir),
            hir_ref_id: value.hir_ref_id.into(),
        }
    }
}

impl<'hir, R1, R2> FromHir<'hir, &'hir Path<'hir, R1>> for TermPath<R2>
where
    R1: Into<R2> + Copy,
{
    fn from_hir(value: &'hir Path<'hir, R1>, hir: Map<'hir>) -> Self {
        Self {
            span: value.span.into(),
            res: value.res.into(),
            segments: value.segments.iter().map(|s| s.hir_into(hir)).collect(),
        }
    }
}

impl<Id1, Id2> From<Res<Id1>> for TermRes<Id2>
where
    Id1: Into<Id2>,
{
    fn from(value: Res<Id1>) -> Self {
        match value {
            Res::Def(dk, did) => Self::Def {
                def: dk.into(),
                id: did.into(),
            },
            Res::PrimTy(ty) => Self::PrimTy { ty: ty.into() },
            Res::SelfTyParam { trait_ } => Self::SelfTyParam {
                trait_: trait_.into(),
            },
            Res::SelfTyAlias {
                alias_to,
                forbid_generic,
                is_trait_impl,
            } => Self::SelfTyAlias {
                alias_to: alias_to.into(),
                forbid_generic,
                is_trait_impl,
            },
            Res::SelfCtor(c) => Self::SelfCtor { id: c.into() },
            Res::Local(l) => Self::Local { id: l.into() },
            Res::ToolMod => Self::ToolMod,
            Res::NonMacroAttr(n) => Self::NonMacroAttr { kind: n.into() },
            Res::Err => Self::Err,
        }
    }
}

impl From<DefKind> for TermDefKind {
    fn from(value: DefKind) -> Self {
        match value {
            DefKind::Mod => Self::Mod,
            DefKind::Struct => Self::Struct,
            DefKind::Union => Self::Union,
            DefKind::Enum => Self::Enum,
            DefKind::Variant => Self::Variant,
            DefKind::Trait => Self::Trait,
            DefKind::TyAlias { .. } => Self::TyAlias,
            DefKind::ForeignTy => Self::ForeignTy,
            DefKind::TraitAlias => Self::TraitAlias,
            DefKind::AssocTy => Self::AssocConst,
            DefKind::TyParam => Self::TyParam,
            DefKind::Fn => Self::Fn,
            DefKind::Const => Self::Const,
            DefKind::ConstParam => Self::ConstParam,
            DefKind::Static { mutability, .. } => Self::Static(mutability.into()),
            DefKind::Ctor(of, kind) => Self::Ctor(of.into(), kind.into()),
            DefKind::AssocFn => Self::AssocFn,
            DefKind::AssocConst => Self::AnonConst,
            DefKind::Macro(k) => Self::Macro(k.into()),
            DefKind::ExternCrate => Self::ExternCrate,
            DefKind::Use => Self::Use,
            DefKind::ForeignMod => Self::ForeignMod,
            DefKind::AnonConst => Self::AnonConst,
            DefKind::InlineConst => Self::InlineConst,
            DefKind::OpaqueTy => Self::OpaqueTy,
            DefKind::Field => Self::Field,
            DefKind::LifetimeParam => Self::LifetimeParam,
            DefKind::GlobalAsm => Self::GlobalAsm,
            DefKind::Impl { of_trait } => Self::Impl { of_trait },
            DefKind::Closure => Self::Closure,
            DefKind::SyntheticCoroutineBody => Self::SyntheticCoroutineBody,
        }
    }
}

impl From<MacroKind> for TermMacroKind {
    fn from(value: MacroKind) -> Self {
        match value {
            MacroKind::Bang => Self::Bang,
            MacroKind::Attr => Self::Attr,
            MacroKind::Derive => Self::Derive,
        }
    }
}

impl From<Mutability> for TermMutability {
    fn from(value: Mutability) -> Self {
        match value {
            Mutability::Not => Self::Not,
            Mutability::Mut => Self::Mut,
        }
    }
}

impl From<CtorOf> for TermCtorOf {
    fn from(value: CtorOf) -> Self {
        match value {
            CtorOf::Struct => Self::Struct,
            CtorOf::Variant => Self::Variant,
        }
    }
}

impl From<CtorKind> for TermCtorKind {
    fn from(value: CtorKind) -> Self {
        match value {
            CtorKind::Fn => Self::Fn,
            CtorKind::Const => Self::Const,
        }
    }
}

impl From<NonMacroAttrKind> for TermNonMacroAttrKind {
    fn from(value: NonMacroAttrKind) -> Self {
        match value {
            NonMacroAttrKind::Builtin(b) => Self::Builtin(b.into()),
            NonMacroAttrKind::Tool => Self::Tool,
            NonMacroAttrKind::DeriveHelper => Self::DeriveHelper,
            NonMacroAttrKind::DeriveHelperCompat => Self::DeriveHelperCompat,
        }
    }
}

impl From<PrimTy> for TermPrimTy {
    fn from(value: PrimTy) -> Self {
        match value {
            PrimTy::Int(i) => Self::Int(i.into()),
            PrimTy::Uint(i) => Self::Uint(i.into()),
            PrimTy::Float(f) => Self::Float(f.into()),
            PrimTy::Str => Self::Str,
            PrimTy::Bool => Self::Bool,
            PrimTy::Char => Self::Char,
        }
    }
}

impl From<IntTy> for TermIntTy {
    fn from(value: IntTy) -> Self {
        match value {
            IntTy::Isize => Self::Isize,
            IntTy::I8 => Self::I8,
            IntTy::I16 => Self::I16,
            IntTy::I32 => Self::I32,
            IntTy::I64 => Self::I64,
            IntTy::I128 => Self::I128,
        }
    }
}

impl From<UintTy> for TermUintTy {
    fn from(value: UintTy) -> Self {
        match value {
            UintTy::Usize => Self::Usize,
            UintTy::U8 => Self::U8,
            UintTy::U16 => Self::U16,
            UintTy::U32 => Self::U32,
            UintTy::U64 => Self::U64,
            UintTy::U128 => Self::U128,
        }
    }
}

impl From<FloatTy> for TermFloatTy {
    fn from(value: FloatTy) -> Self {
        match value {
            FloatTy::F16 => Self::F16,
            FloatTy::F32 => Self::F32,
            FloatTy::F64 => Self::F64,
            FloatTy::F128 => Self::F128,
        }
    }
}

enum StubKind {
    Exists,
    Forall,
    Equiv,
}

enum TraitFn {
    OrdLt,
    OrdLe,
    OrdGe,
    OrdGt,
    Index,
    ShallowModel,
    DeepModel,
}

enum RmlFnKind {
    Stub(StubKind),
    TraitFn(TraitFn),
}

impl RmlFnKind {
    fn get_quant_kind(&self) -> Option<QuantorKind> {
        match self {
            Self::Stub(StubKind::Exists) => Some(QuantorKind::Exists),
            Self::Stub(StubKind::Forall) => Some(QuantorKind::Forall),
            _ => None,
        }
    }
}

fn is_rml_fn(segments: &[PathSegment]) -> bool {
    (segments.len() == 4
        && segments[0].ident.name == Symbol::intern("{{root}}")
        && segments[1].ident.name == Symbol::intern("rml_contracts")
        && segments[2].ident.name == Symbol::intern("stubs"))
        || (segments.len() == 5
            && segments[0].ident.name == Symbol::intern("{{root}}")
            && segments[1].ident.name == Symbol::intern("rml_contracts")
            && segments[2].ident.name == Symbol::intern("logic"))
}

fn get_rml_fn_kind(segments: &[PathSegment]) -> Option<RmlFnKind> {
    let kind = segments[2].ident.name;
    if kind == Symbol::intern("stubs") {
        let name = segments[3].ident.name;
        if name == Symbol::intern("exists") {
            Some(RmlFnKind::Stub(StubKind::Exists))
        } else if name == Symbol::intern("forall") {
            Some(RmlFnKind::Stub(StubKind::Forall))
        } else if name == Symbol::intern("equiv") {
            Some(RmlFnKind::Stub(StubKind::Equiv))
        } else {
            None
        }
    } else if kind == Symbol::intern("logic") {
        let r#trait = segments[3].ident.as_str();
        let r#fn = segments[4].ident.as_str();

        match (r#trait, r#fn) {
            ("OrdLogic", "lt_log") => Some(RmlFnKind::TraitFn(TraitFn::OrdLt)),
            ("OrdLogic", "le_log") => Some(RmlFnKind::TraitFn(TraitFn::OrdLe)),
            ("OrdLogic", "ge_log") => Some(RmlFnKind::TraitFn(TraitFn::OrdGe)),
            ("OrdLogic", "gt_log") => Some(RmlFnKind::TraitFn(TraitFn::OrdGt)),
            ("ShallowModel", "shallow_model") => Some(RmlFnKind::TraitFn(TraitFn::ShallowModel)),
            ("DeepModel", "DeepModel") => Some(RmlFnKind::TraitFn(TraitFn::DeepModel)),
            ("IndexLogic", "index_logic") => Some(RmlFnKind::TraitFn(TraitFn::Index)),
            _ => panic!("Unknown trait {} or function {}", r#trait, r#fn),
        }
    } else {
        None
    }
}
