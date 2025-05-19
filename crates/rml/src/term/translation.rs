//! Handle translation of HIR structures to [Term], and the requires
//! sub-structures.
//!
//! See especially: [FromHir], [HirInto].

use rustc_abi::ExternAbi;
use rustc_ast::{
    BindingMode, BorrowKind, ByRef, CaptureBy, FloatTy, IntTy, LitFloatType, LitIntType, LitKind,
    Mutability, StrStyle, TraitObjectSyntax, UintTy,
};
use rustc_hir::{
    def::{CtorKind, CtorOf, DefKind, NonMacroAttrKind, Res},
    AngleBrackets, AnonConst, Arm, BareFnTy, BinOp, BinOpKind, Block, BlockCheckMode, Body,
    Closure, ClosureBinder, ConstArg, ConstArgKind, ConstBlock, Constness, DotDotPos, Expr,
    ExprField, ExprKind, FnDecl, FnRetTy, GenericArg, GenericArgs, GenericArgsParentheses,
    GenericBound, GenericParam, GenericParamKind, GenericParamSource, ImplicitSelfKind, InferArg,
    LetExpr, LetStmt, Lifetime, LifetimeKind, LifetimeParamKind, LifetimeSource, LifetimeSyntax,
    Lit, MatchSource, MutTy, OpaqueTy, Param, ParamName, Pat, PatExpr, PatExprKind, PatField,
    PatKind, Path, PathSegment, PolyTraitRef, PrimTy, QPath, RangeEnd, Stmt, StmtKind,
    StructTailExpr, TraitRef, Ty, TyKind, TyPat, TyPatKind, UnOp, UnsafeSource,
};
use rustc_middle::ty::TyCtxt;
use rustc_span::{MacroKind, Symbol};

use super::{
    HirTerm, PatTerm, PatTermKind, QuantorKind, QuantorParam, StructTailTerm, Term, TermAbi,
    TermAngleBrackets, TermAnonConst, TermArm, TermBareFnTy, TermBinOp, TermBinOpKind,
    TermBindingMode, TermBlock, TermBlockCheckMode, TermBody, TermBorrowKind, TermByRef,
    TermCaptureBy, TermClosure, TermClosureBinder, TermConstArg, TermConstArgKind, TermConstBlock,
    TermConstness, TermCtorKind, TermCtorOf, TermDefKind, TermDotDotPos, TermField, TermFloatTy,
    TermFnDecl, TermFnRetTy, TermGenericArg, TermGenericArgs, TermGenericArgsParentheses,
    TermGenericBound, TermGenericParam, TermGenericParamKind, TermGenericParamSource,
    TermImplicitSelfKind, TermInferArg, TermIntTy, TermKind, TermLet, TermLetStmt, TermLifetime,
    TermLifetimeKind, TermLifetimeParamKind, TermLifetimeSource, TermLifetimeSyntax, TermLit,
    TermLitFloatType, TermLitIntType, TermLitKind, TermMacroKind, TermMatchSource, TermMutTy,
    TermMutability, TermNonMacroAttrKind, TermOpaqueTy, TermParam, TermParamName, TermPat,
    TermPatField, TermPatKind, TermPath, TermPathSegment, TermPolyTraitRef, TermPrimTy, TermQPath,
    TermRangeEnd, TermRes, TermStmt, TermStmtKind, TermStrStyle, TermTraitObjectSyntax,
    TermTraitRef, TermTy, TermTyKind, TermTyPat, TermTyPatKind, TermUintTy, TermUnOp,
    TermUnsafeSource,
};

/// Allows translating from `T` to `Self`, where `T` is a HIR structure. Since
/// some structures reference bodies, we require access to the HIR.
pub trait FromHir<'tcx, T>
where
    T: Sized,
{
    /// Translate from `value` to `Self`, where `T` is a HIR structure. Since
    /// some structures reference bodies, we require access to the HIR via
    /// `tcx`.
    fn from_hir(value: T, tcx: TyCtxt<'tcx>) -> Self;
}

/// Allows translating from `Self` to `T`, where `Self` is a HIR structure.
/// Since some structures reference bodies, we require access to the HIR.
///
/// **Do not implement this directly.** Use [FromHir] instead.
pub trait HirInto<'tcx, T>
where
    T: Sized,
{
    /// Translate from `self` to `T`, where `self` is a HIR structure. Since
    /// some structures reference bodies, we require access to the HIR via
    /// `tcx`.
    fn hir_into(self, tcx: TyCtxt<'tcx>) -> T;
}

impl<'tcx, T, U> HirInto<'tcx, U> for T
where
    U: FromHir<'tcx, T>,
{
    fn hir_into(self, tcx: TyCtxt<'tcx>) -> U {
        U::from_hir(self, tcx)
    }
}

impl<'tcx> FromHir<'tcx, &'tcx Expr<'tcx>> for Term {
    fn from_hir(value: &'tcx Expr<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            kind: value.kind.hir_into(tcx),
        }
    }
}

impl<'tcx> FromHir<'tcx, ExprKind<'tcx>> for TermKind {
    fn from_hir(value: ExprKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            ExprKind::Array(exprs) => Self::Array {
                terms: exprs.iter().map(|e| e.hir_into(tcx)).collect(),
            },
            ExprKind::Call(recv, args) => match recv.kind {
                ExprKind::Path(QPath::Resolved(None, Path { segments, .. }))
                    if is_rml_fn(segments) =>
                {
                    let kind = get_rml_fn_kind(segments).expect("expected valid rml function");

                    if let Some(qkind) = kind.get_quant_kind() {
                        let clo: TermClosure = match args[0].kind {
                            ExprKind::Closure(c) => c.hir_into(tcx),
                            _ => unreachable!(),
                        };
                        let param = {
                            let c_param = &clo.body.params[0];

                            let ident = match &c_param.pat.kind {
                                TermPatKind::Binding {
                                    mode: _,
                                    hir_id: _,
                                    ident,
                                    pat: _,
                                } => ident.clone(),
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
                            let left: Term = (&args[0]).hir_into(tcx);
                            let right: Term = (&args[1]).hir_into(tcx);

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
                            let term: Term = (&args[0]).hir_into(tcx);
                            TermKind::Model {
                                kind: super::ModelKind::Shallow,
                                term: term.into(),
                            }
                        }
                        RmlFnKind::TraitFn(TraitFn::DeepModel) => {
                            let term: Term = (&args[0]).hir_into(tcx);
                            TermKind::Model {
                                kind: super::ModelKind::Deep,
                                term: term.into(),
                            }
                        }
                        RmlFnKind::TraitFn(TraitFn::Index) => {
                            let term: Term = (&args[0]).hir_into(tcx);
                            let index: Term = (&args[1]).hir_into(tcx);
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
                            let left: Term = (&args[0]).hir_into(tcx);
                            let right: Term = (&args[1]).hir_into(tcx);

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
                    callee: Box::new(recv.hir_into(tcx)),
                    args: args.iter().map(|a| a.hir_into(tcx)).collect(),
                },
            },
            ExprKind::MethodCall(path, recv, args, span) => Self::MethodCall {
                path: path.hir_into(tcx),
                callee: Box::new(recv.hir_into(tcx)),
                args: args.iter().map(|a| a.hir_into(tcx)).collect(),
                span: span.into(),
            },
            ExprKind::Tup(exprs) => Self::Tup {
                terms: exprs.iter().map(|e| e.hir_into(tcx)).collect(),
            },
            ExprKind::Binary(op, left, right) => Self::Binary {
                op: op.into(),
                left: Box::new(left.hir_into(tcx)),
                right: Box::new(right.hir_into(tcx)),
            },
            ExprKind::Unary(op, expr) => Self::Unary {
                op: op.into(),
                child: Box::new(expr.hir_into(tcx)),
            },
            ExprKind::Lit(lit) => Self::Lit { lit: lit.into() },
            ExprKind::Cast(expr, ty) => Self::Cast {
                term: Box::new(expr.hir_into(tcx)),
                ty: ty.hir_into(tcx),
            },
            ExprKind::Let(l) => Self::Let {
                r#let: l.hir_into(tcx),
            },
            ExprKind::If(cond, then, els) => Self::If {
                cond: Box::new(cond.hir_into(tcx)),
                then: Box::new(then.hir_into(tcx)),
                r#else: els.map(|e| Box::new(e.hir_into(tcx))),
            },
            ExprKind::Match(expr, arms, source) => Self::Match {
                term: Box::new(expr.hir_into(tcx)),
                arms: arms.iter().map(|a| a.hir_into(tcx)).collect(),
                src: source.into(),
            },
            ExprKind::Closure(c) => Self::Closure {
                closure: c.hir_into(tcx),
            },
            ExprKind::Block(b, _) => Self::Block {
                block: b.hir_into(tcx),
            },
            ExprKind::Field(expr, field) => Self::Field {
                term: Box::new(expr.hir_into(tcx)),
                field: field.into(),
            },
            ExprKind::Index(expr, idx, span) => Self::Index {
                term: Box::new(expr.hir_into(tcx)),
                idx: Box::new(idx.hir_into(tcx)),
                span: span.into(),
            },
            ExprKind::Path(p) => Self::Path {
                path: p.hir_into(tcx),
            },
            ExprKind::AddrOf(brw, m, expr) => Self::AddrOf {
                kind: brw.into(),
                mutability: m.into(),
                term: Box::new(expr.hir_into(tcx)),
            },
            ExprKind::Struct(p, fields, rest) => Self::Struct {
                path: p.hir_into(tcx),
                fields: fields.iter().map(|f| f.hir_into(tcx)).collect(),
                rest: rest.hir_into(tcx),
            },
            ExprKind::Repeat(expr, len) => Self::Repeat {
                term: Box::new(expr.hir_into(tcx)),
                len: Box::new(len.hir_into(tcx)),
            },
            k => panic!("Unsupported kind {k:?}"),
        }
    }
}

impl<'tcx> FromHir<'tcx, StructTailExpr<'tcx>> for StructTailTerm {
    fn from_hir(value: StructTailExpr<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            StructTailExpr::None => StructTailTerm::None,
            StructTailExpr::Base(expr) => StructTailTerm::Base(Box::new(expr.hir_into(tcx))),
            StructTailExpr::DefaultFields(span) => StructTailTerm::DefaultFields(span.into()),
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

impl<'tcx> FromHir<'tcx, &'tcx LetExpr<'tcx>> for TermLet {
    fn from_hir(value: &'tcx LetExpr<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            span: value.span.into(),
            pat: value.pat.hir_into(tcx),
            ty: value.ty.map(|t| t.hir_into(tcx)),
            init: Box::new(value.init.hir_into(tcx)),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx LetStmt<'tcx>> for TermLetStmt {
    fn from_hir(value: &'tcx LetStmt<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            pat: value.pat.hir_into(tcx),
            ty: value.ty.map(|ty| ty.hir_into(tcx)),
            init: Box::new(value.init.unwrap().hir_into(tcx)),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx Arm<'tcx>> for TermArm {
    fn from_hir(value: &'tcx Arm<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            pat: value.pat.hir_into(tcx),
            guard: value.guard.map(|g| g.hir_into(tcx)),
            body: Box::new(value.body.hir_into(tcx)),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx Closure<'tcx>> for TermClosure {
    fn from_hir(value: &'tcx Closure<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            def_id: value.def_id.into(),
            binder: value.binder.into(),
            constness: value.constness.into(),
            capture_clause: value.capture_clause.into(),
            bound_generic_params: value
                .bound_generic_params
                .iter()
                .map(|p| p.hir_into(tcx))
                .collect(),
            fn_decl: value.fn_decl.hir_into(tcx),
            body: tcx.hir_body(value.body).hir_into(tcx),
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
            CaptureBy::Use { use_kw } => Self::Use {
                use_kw: use_kw.into(),
            },
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx FnDecl<'tcx>> for TermFnDecl {
    fn from_hir(value: &'tcx FnDecl<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            inputs: value.inputs.iter().map(|i| i.hir_into(tcx)).collect(),
            output: value.output.hir_into(tcx),
            c_variadic: value.c_variadic,
            implicit_self: value.implicit_self.into(),
            lifetime_elision_allowed: value.lifetime_elision_allowed,
        }
    }
}

impl<'tcx> FromHir<'tcx, FnRetTy<'tcx>> for TermFnRetTy {
    fn from_hir(value: FnRetTy<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            FnRetTy::DefaultReturn(sp) => Self::DefaultReturn(sp.into()),
            FnRetTy::Return(ty) => Self::Return(Box::new(ty.hir_into(tcx))),
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

impl<'tcx> FromHir<'tcx, &'tcx Block<'tcx>> for TermBlock {
    fn from_hir(value: &'tcx Block<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            stmts: value.stmts.iter().map(|s| s.hir_into(tcx)).collect(),
            term: value.expr.map(|e| Box::new(e.hir_into(tcx))),
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

impl<'tcx> FromHir<'tcx, &'tcx ExprField<'tcx>> for TermField {
    fn from_hir(value: &'tcx ExprField<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            term: value.expr.hir_into(tcx),
            span: value.span.into(),
            is_shorthand: value.is_shorthand,
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx Stmt<'tcx>> for TermStmt {
    fn from_hir(value: &'tcx Stmt<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: value.kind.hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, StmtKind<'tcx>> for TermStmtKind {
    fn from_hir(value: StmtKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            StmtKind::Let(l) => Self::Let(l.hir_into(tcx)),
            StmtKind::Item(i) => Self::Item(i.into()),
            StmtKind::Expr(e) => Self::Term(Box::new(e.hir_into(tcx))),
            StmtKind::Semi(e) => Self::Semi(Box::new(e.hir_into(tcx))),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx Body<'tcx>> for TermBody {
    fn from_hir(value: &'tcx Body<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            params: value.params.iter().map(|p| p.hir_into(tcx)).collect(),
            value: Box::new(value.value.hir_into(tcx)),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx Param<'tcx>> for TermParam {
    fn from_hir(value: &'tcx Param, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            pat: value.pat.hir_into(tcx),
            ty_span: value.ty_span.into(),
            span: value.span.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx Pat<'tcx>> for TermPat {
    fn from_hir(value: &'tcx Pat<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: value.kind.hir_into(tcx),
            span: value.span.into(),
            default_binding_modes: value.default_binding_modes,
        }
    }
}

impl<'tcx> FromHir<'tcx, PatKind<'tcx>> for TermPatKind {
    fn from_hir(value: PatKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            PatKind::Wild => Self::Wild,
            PatKind::Binding(ann, hid, ident, pat) => Self::Binding {
                mode: ann.into(),
                hir_id: hid.into(),
                ident: ident.into(),
                pat: pat.map(|p| Box::new(p.hir_into(tcx))),
            },
            PatKind::Struct(qp, fields, rest) => Self::Struct {
                path: qp.hir_into(tcx),
                fields: fields.iter().map(|f| f.hir_into(tcx)).collect(),
                rest,
            },
            PatKind::TupleStruct(qp, elems, ddp) => Self::TupleStruct {
                path: qp.hir_into(tcx),
                pats: elems.iter().map(|e| e.hir_into(tcx)).collect(),
                dot_dot_pos: ddp.into(),
            },
            PatKind::Or(pats) => Self::Or {
                pats: pats.iter().map(|p| p.hir_into(tcx)).collect(),
            },
            PatKind::Tuple(pats, ddp) => Self::Tuple {
                pats: pats.iter().map(|p| p.hir_into(tcx)).collect(),
                dot_dot_pos: ddp.into(),
            },
            PatKind::Box(p) => Self::Box {
                pat: Box::new(p.hir_into(tcx)),
            },
            PatKind::Ref(p, m) => Self::Ref {
                pat: Box::new(p.hir_into(tcx)),
                mutability: m.into(),
            },
            PatKind::Range(from, to, re) => Self::Range {
                lhs: from.map(|f| Box::new(f.hir_into(tcx))),
                rhs: to.map(|t| Box::new(t.hir_into(tcx))),
                range: re.into(),
            },
            PatKind::Slice(start, mid, end) => Self::Slice {
                start: start.iter().map(|p| p.hir_into(tcx)).collect(),
                mid: mid.map(|m| Box::new(m.hir_into(tcx))),
                rest: end.iter().map(|p| p.hir_into(tcx)).collect(),
            },
            PatKind::Never => Self::Never,
            PatKind::Deref(pat) => Self::Deref {
                pat: Box::new(pat.hir_into(tcx)),
            },
            PatKind::Err(_) => Self::Err,
            PatKind::Missing => Self::Missing,
            PatKind::Expr(pat_expr) => Self::Term {
                term: pat_expr.hir_into(tcx),
            },
            PatKind::Guard(pat, expr) => Self::Guard {
                pat: Box::new(pat.hir_into(tcx)),
                guard: Box::new(expr.hir_into(tcx)),
            },
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx PatExpr<'tcx>> for PatTerm {
    fn from_hir(value: &'tcx PatExpr<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            kind: value.kind.hir_into(tcx),
        }
    }
}

impl<'tcx> FromHir<'tcx, PatExprKind<'tcx>> for PatTermKind {
    fn from_hir(value: PatExprKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            PatExprKind::Lit { lit, negated } => Self::Lit {
                lit: lit.into(),
                negated,
            },
            PatExprKind::ConstBlock(const_block) => Self::ConstBlock(const_block.hir_into(tcx)),
            PatExprKind::Path(qpath) => Self::Path(qpath.hir_into(tcx)),
        }
    }
}

impl<'tcx> FromHir<'tcx, ConstBlock> for TermConstBlock {
    fn from_hir(value: ConstBlock, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            body: tcx.hir_body(value.body).hir_into(tcx),
        }
    }
}

impl From<BindingMode> for TermBindingMode {
    fn from(value: BindingMode) -> Self {
        Self {
            by_ref: value.0.into(),
            r#mut: value.1.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx PatField<'tcx>> for TermPatField {
    fn from_hir(value: &'tcx PatField<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            pat: value.pat.hir_into(tcx),
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
            ByRef::Yes(Mutability::Mut) => Self::Yes { r#mut: true },
            ByRef::Yes(Mutability::Not) => Self::Yes { r#mut: false },
            ByRef::No => Self::No,
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx PathSegment<'tcx>> for TermPathSegment {
    fn from_hir(value: &PathSegment<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            ident: value.ident.into(),
            hir_id: value.hir_id.into(),
            res: value.res.into(),
            args: value.args.map(|a| a.hir_into(tcx)),
            infer_args: value.infer_args,
        }
    }
}

impl<'tcx, A> FromHir<'tcx, &'tcx Ty<'tcx, A>> for TermTy {
    fn from_hir(value: &'tcx Ty<'tcx, A>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: (&value.kind).hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'tcx, A> FromHir<'tcx, &'tcx TyKind<'tcx, A>> for TermTyKind {
    fn from_hir(value: &'tcx TyKind<'tcx, A>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            TyKind::Slice(ty) => Self::Slice(Box::new((*ty).hir_into(tcx))),
            TyKind::Array(ty, len) => Self::Array(
                Box::new((*ty).hir_into(tcx)),
                Box::new((*len).hir_into(tcx)),
            ),
            TyKind::Ptr(mty) => Self::Ptr(Box::new((*mty).hir_into(tcx))),
            TyKind::Ref(l, mty) => Self::Ref((*l).into(), Box::new((*mty).hir_into(tcx))),
            TyKind::BareFn(f) => Self::BareFn((*f).hir_into(tcx)),
            TyKind::Never => Self::Never,
            TyKind::Tup(tys) => Self::Tup(tys.iter().map(|ty| ty.hir_into(tcx)).collect()),
            TyKind::Path(p) => Self::Path(p.hir_into(tcx)),
            TyKind::OpaqueDef(ty) => Self::OpaqueDef((*ty).hir_into(tcx)),
            TyKind::TraitObject(prefs, tagged) => Self::TraitObject(
                prefs.iter().map(|r| r.hir_into(tcx)).collect(),
                tagged.pointer().into(),
                tagged.tag().into(),
            ),
            TyKind::Typeof(c) => Self::Typeof(Box::new((*c).hir_into(tcx))),
            TyKind::Infer(..) => Self::Infer,
            TyKind::InferDelegation(did, _) => Self::InferDelegation((*did).into()),
            TyKind::Pat(ty, pat) => Self::Pat(Box::new((*ty).hir_into(tcx)), (*pat).hir_into(tcx)),
            TyKind::Err(_) => unreachable!(),
            TyKind::UnsafeBinder(_unsafe_binder_ty) => todo!(),
            TyKind::TraitAscription(_generic_bounds) => todo!(),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx TyPat<'tcx>> for TermTyPat {
    fn from_hir(value: &'tcx TyPat<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: (&value.kind).hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx TyPatKind<'tcx>> for TermTyPatKind {
    fn from_hir(value: &'tcx TyPatKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            TyPatKind::Range(p, p1) => Self::Range {
                start: Box::new((*p).hir_into(tcx)),
                end: Box::new((*p1).hir_into(tcx)),
            },
            TyPatKind::Or(thin_vec) => Self::Or {
                pats: thin_vec.iter().map(|p| p.hir_into(tcx)).collect(),
            },
            TyPatKind::Err(_) => Self::Err,
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx OpaqueTy<'tcx>> for TermOpaqueTy {
    fn from_hir(value: &'tcx OpaqueTy<'tcx>, _tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            span: value.span.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, MutTy<'tcx>> for TermMutTy {
    fn from_hir(value: MutTy<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            ty: value.ty.hir_into(tcx),
            mutbl: value.mutbl.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx BareFnTy<'tcx>> for TermBareFnTy {
    fn from_hir(value: &'tcx BareFnTy<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            abi: value.abi.into(),
            generic_params: value
                .generic_params
                .iter()
                .map(|p| p.hir_into(tcx))
                .collect(),
            decl: value.decl.hir_into(tcx),
            param_idents: value
                .param_idents
                .iter()
                .map(|ident| ident.map(|i| i.into()))
                .collect(),
        }
    }
}

impl From<ExternAbi> for TermAbi {
    fn from(value: ExternAbi) -> Self {
        match value {
            ExternAbi::Rust => Self::Rust,
            ExternAbi::C { unwind } => Self::C { unwind },
            ExternAbi::Cdecl { unwind } => Self::Cdecl { unwind },
            ExternAbi::Stdcall { unwind } => Self::Stdcall { unwind },
            ExternAbi::Fastcall { unwind } => Self::Fastcall { unwind },
            ExternAbi::Vectorcall { unwind } => Self::Vectorcall { unwind },
            ExternAbi::Thiscall { unwind } => Self::Thiscall { unwind },
            ExternAbi::Aapcs { unwind } => Self::Aapcs { unwind },
            ExternAbi::Win64 { unwind } => Self::Win64 { unwind },
            ExternAbi::SysV64 { unwind } => Self::SysV64 { unwind },
            ExternAbi::PtxKernel => Self::PtxKernel,
            ExternAbi::Msp430Interrupt => Self::Msp430Interrupt,
            ExternAbi::X86Interrupt => Self::X86Interrupt,
            ExternAbi::EfiApi => Self::EfiApi,
            ExternAbi::AvrInterrupt => Self::AvrNonBlockingInterrupt,
            ExternAbi::AvrNonBlockingInterrupt => Self::AvrNonBlockingInterrupt,
            ExternAbi::CCmseNonSecureCall => Self::CCmseNonSecureCall,
            ExternAbi::System { unwind } => Self::System { unwind },
            ExternAbi::RustCall => Self::RustCall,
            ExternAbi::Unadjusted => Self::Unadjusted,
            ExternAbi::RustCold => Self::RustCold,
            ExternAbi::RiscvInterruptM => Self::RiscvInterruptM,
            ExternAbi::RiscvInterruptS => Self::RiscvInterruptS,
            ExternAbi::GpuKernel => Self::GpuKernel,
            ExternAbi::CCmseNonSecureEntry => Self::CCmseNonSecureEntry,
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

impl<'tcx> From<&'tcx Lit> for TermLit {
    fn from(value: &'tcx Lit) -> Self {
        Self {
            node: (&value.node).into(),
            span: value.span.into(),
        }
    }
}

impl<'tcx> From<&'tcx LitKind> for TermLitKind {
    fn from(value: &'tcx LitKind) -> Self {
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

impl<'tcx> From<&'tcx StrStyle> for TermStrStyle {
    fn from(value: &'tcx StrStyle) -> Self {
        match value {
            StrStyle::Cooked => Self::Cooked,
            StrStyle::Raw(r) => Self::Raw { number: *r },
        }
    }
}

impl<'tcx> From<&'tcx LitIntType> for TermLitIntType {
    fn from(value: &'tcx LitIntType) -> Self {
        match value {
            LitIntType::Signed(s) => Self::Signed { ty: (*s).into() },
            LitIntType::Unsigned(u) => Self::Unsigned { ty: (*u).into() },
            LitIntType::Unsuffixed => Self::Unsuffixed,
        }
    }
}

impl<'tcx> From<&'tcx LitFloatType> for TermLitFloatType {
    fn from(value: &'tcx LitFloatType) -> Self {
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

impl<'tcx> FromHir<'tcx, QPath<'tcx>> for TermQPath {
    fn from_hir(value: QPath<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            QPath::Resolved(ty, path) => Self::Resolved {
                ty: ty.map(|ty| Box::new(ty.hir_into(tcx))),
                path: path.hir_into(tcx),
            },
            QPath::TypeRelative(ty, ps) => Self::TypeRelative {
                ty: Box::new(ty.hir_into(tcx)),
                seg: ps.hir_into(tcx),
            },
            QPath::LangItem(li, sp) => Self::LangItem {
                item: (&li).into(),
                span: sp.into(),
            },
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx QPath<'tcx>> for TermQPath {
    fn from_hir(value: &'tcx QPath<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        (*value).hir_into(tcx)
    }
}

impl<'tcx> FromHir<'tcx, &'tcx GenericArgs<'tcx>> for TermGenericArgs {
    fn from_hir(value: &'tcx GenericArgs<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            args: value.args.iter().map(|a| a.hir_into(tcx)).collect(),
            // bindings: value.bindings.iter().map(|b| b.hir_into(hir)).collect(),
            parenthesized: value.parenthesized.into(),
            span_ext: value.span_ext.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx GenericArg<'tcx>> for TermGenericArg {
    fn from_hir(value: &'tcx GenericArg<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            GenericArg::Lifetime(l) => Self::Lifetime((*l).into()),
            GenericArg::Type(ty) => Self::Type((*ty).hir_into(tcx)),
            GenericArg::Const(c) => Self::Const((*c).hir_into(tcx)),
            GenericArg::Infer(i) => Self::Infer(i.into()),
        }
    }
}

impl<'tcx> From<&'tcx Lifetime> for TermLifetime {
    fn from(value: &'tcx Lifetime) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            ident: value.ident.into(),
            kind: value.kind.into(),
            source: value.source.into(),
            syntax: value.syntax.into(),
        }
    }
}

impl From<LifetimeKind> for TermLifetimeKind {
    fn from(value: LifetimeKind) -> Self {
        match value {
            LifetimeKind::Param(p) => Self::Param(p.into()),
            LifetimeKind::ImplicitObjectLifetimeDefault => Self::ImplicitObjectLifetimeDefault,
            LifetimeKind::Error => unreachable!(),
            LifetimeKind::Infer => Self::Infer,
            LifetimeKind::Static => Self::Static,
        }
    }
}

impl From<LifetimeSource> for TermLifetimeSource {
    fn from(value: LifetimeSource) -> Self {
        match value {
            LifetimeSource::Reference => Self::Reference,
            LifetimeSource::Path { angle_brackets } => Self::Path {
                angle_brackets: angle_brackets.into(),
            },
            LifetimeSource::OutlivesBound => Self::OutlivesBound,
            LifetimeSource::PreciseCapturing => Self::PreciseCapturing,
            LifetimeSource::Other => Self::Other,
        }
    }
}

impl From<LifetimeSyntax> for TermLifetimeSyntax {
    fn from(value: LifetimeSyntax) -> Self {
        match value {
            LifetimeSyntax::Hidden => Self::Hidden,
            LifetimeSyntax::Anonymous => Self::Anonymous,
            LifetimeSyntax::Named => Self::Named,
        }
    }
}

impl From<AngleBrackets> for TermAngleBrackets {
    fn from(value: AngleBrackets) -> Self {
        match value {
            AngleBrackets::Missing => Self::Missing,
            AngleBrackets::Empty => Self::Empty,
            AngleBrackets::Full => Self::Full,
        }
    }
}

impl<'tcx, A> FromHir<'tcx, &'tcx ConstArg<'tcx, A>> for TermConstArg {
    fn from_hir(value: &'tcx ConstArg<'tcx, A>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: (&value.kind).hir_into(tcx),
        }
    }
}

impl<'tcx, A> FromHir<'tcx, &'tcx ConstArgKind<'tcx, A>> for TermConstArgKind {
    fn from_hir(value: &'tcx ConstArgKind<'tcx, A>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            ConstArgKind::Path(q) => Self::Path(q.hir_into(tcx)),
            ConstArgKind::Anon(a) => Self::Anon((*a).hir_into(tcx)),
            ConstArgKind::Infer(..) => Self::Infer,
        }
    }
}

impl<'tcx> From<&'tcx InferArg> for TermInferArg {
    fn from(value: &'tcx InferArg) -> Self {
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

impl<'tcx> FromHir<'tcx, rustc_hir::Term<'tcx>> for HirTerm {
    fn from_hir(value: rustc_hir::Term<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            rustc_hir::Term::Ty(ty) => Self::Ty(ty.hir_into(tcx)),
            rustc_hir::Term::Const(c) => Self::Const(c.hir_into(tcx)),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx GenericBound<'tcx>> for TermGenericBound {
    fn from_hir(value: &'tcx GenericBound<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            GenericBound::Trait(pr) => Self::Trait(pr.hir_into(tcx)),
            GenericBound::Outlives(l) => Self::Outlives((*l).into()),
            GenericBound::Use(..) => todo!("GenericBound::Use"),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx PolyTraitRef<'tcx>> for TermPolyTraitRef {
    fn from_hir(value: &'tcx PolyTraitRef<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            bound_generic_params: value
                .bound_generic_params
                .iter()
                .map(|p| p.hir_into(tcx))
                .collect(),
            trait_ref: value.trait_ref.hir_into(tcx),
            span: value.span.into(),
        }
    }
}

impl<'tcx> FromHir<'tcx, &'tcx GenericParam<'tcx>> for TermGenericParam {
    fn from_hir(value: &'tcx GenericParam<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            name: value.name.into(),
            span: value.span.into(),
            pure_wrt_drop: value.pure_wrt_drop,
            kind: value.kind.hir_into(tcx),
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
            ParamName::Error(_) => unreachable!(),
        }
    }
}

impl<'tcx> FromHir<'tcx, GenericParamKind<'tcx>> for TermGenericParamKind {
    fn from_hir(value: GenericParamKind<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match value {
            GenericParamKind::Lifetime { kind } => Self::Lifetime { kind: kind.into() },
            GenericParamKind::Type { default, synthetic } => Self::Type {
                default: default.map(|ty| ty.hir_into(tcx)),
                synthetic,
            },
            GenericParamKind::Const {
                ty,
                default,
                synthetic,
            } => Self::Const {
                ty: ty.hir_into(tcx),
                default: default.map(|ac| ac.hir_into(tcx)),
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

impl<'tcx> FromHir<'tcx, &'tcx AnonConst> for TermAnonConst {
    fn from_hir(value: &'tcx AnonConst, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            def_id: value.def_id.into(),
            body: tcx.hir_body(value.body).value.hir_into(tcx),
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

impl<'tcx> FromHir<'tcx, TraitRef<'tcx>> for TermTraitRef {
    fn from_hir(value: TraitRef<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            path: value.path.hir_into(tcx),
            hir_ref_id: value.hir_ref_id.into(),
        }
    }
}

impl<'tcx, R1, R2> FromHir<'tcx, &'tcx Path<'tcx, R1>> for TermPath<R2>
where
    R1: Into<R2> + Copy,
{
    fn from_hir(value: &'tcx Path<'tcx, R1>, tcx: TyCtxt<'tcx>) -> Self {
        Self {
            span: value.span.into(),
            res: value.res.into(),
            segments: value.segments.iter().map(|s| s.hir_into(tcx)).collect(),
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
