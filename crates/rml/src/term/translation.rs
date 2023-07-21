use rustc_hir::{
    Arm, BinOp, Block, Body, Closure, Expr, ExprField, ExprKind, Guard, Let, Lit, Local,
    MatchSource, PatKind, Path, PathSegment, QPath, Stmt, StmtKind, Ty, TyKind, UnOp,
};
use rustc_middle::hir::map::Map;
use rustc_span::Symbol;

use super::{
    QuantorKind, QuantorParam, Term, TermArm, TermBinOp, TermBlock, TermBody, TermClosure,
    TermField, TermGuard, TermKind, TermLet, TermLit, TermLocal, TermMatchSource, TermPatKind,
    TermPathSegment, TermQPath, TermStmt, TermStmtKind, TermTy, TermTyKind, TermUnOp,
};

pub trait FromHir<'hir, T>
where
    T: Sized,
{
    fn from_hir(value: T, hir: Map<'hir>) -> Self;
}

pub trait HirInto<'hir, T>
where
    T: Sized,
{
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
            ExprKind::Array(exprs) => {
                Self::Array(exprs.into_iter().map(|e| e.hir_into(hir)).collect())
            }
            ExprKind::Call(recv, args) => match recv.kind {
                ExprKind::Path(QPath::Resolved(None, Path { segments, .. }))
                    if is_stub(segments) =>
                {
                    let kind = get_stub_kind(segments).expect("expected valid stub function");
                    let qkind = match kind {
                        StubKind::Exists => QuantorKind::Exists,
                        StubKind::Forall => QuantorKind::Forall,
                    };

                    let clo: TermClosure = match args[0].kind {
                        ExprKind::Closure(c) => c.hir_into(hir),
                        _ => unreachable!(),
                    };
                    let param = {
                        let c_param = clo.body.params[0];

                        let ident = match c_param.pat.kind {
                            TermPatKind::Binding(_, _, ident, _) => ident,
                            _ => unreachable!(),
                        };

                        QuantorParam {
                            hir_id: c_param.hir_id,
                            ident,
                            ty: clo.fn_decl.inputs[0],
                            span: c_param.span,
                            ty_span: c_param.ty_span,
                        }
                    };

                    let term = clo.body.value;

                    Self::Quantor(qkind, param, term)
                }
                _ => Self::Call(
                    Box::new(recv.hir_into(hir)),
                    args.into_iter().map(|a| a.hir_into(hir)).collect(),
                ),
            },
            ExprKind::MethodCall(path, recv, args, span) => Self::MethodCall(
                path.into(),
                Box::new(recv.hir_into(hir)),
                args.into_iter().map(|a| a.hir_into(hir)).collect(),
                span.into(),
            ),
            ExprKind::Tup(exprs) => Self::Tup(exprs.into_iter().map(|e| e.hir_into(hir)).collect()),
            ExprKind::Binary(op, left, right) => Self::Binary(
                op.into(),
                Box::new(left.hir_into(hir)),
                Box::new(right.hir_into(hir)),
            ),
            ExprKind::Unary(op, expr) => Self::Unary(op.into(), Box::new(expr.hir_into(hir))),
            ExprKind::Lit(lit) => Self::Lit(lit.into()),
            ExprKind::Cast(expr, ty) => Self::Cast(Box::new(expr.hir_into(hir)), ty.into()),
            ExprKind::Let(l) => Self::Let(l.hir_into(hir)),
            ExprKind::If(cond, then, els) => Self::If(
                Box::new(cond.hir_into(hir)),
                Box::new(then.hir_into(hir)),
                els.map(|e| Box::new(e.hir_into(hir))),
            ),
            ExprKind::Match(expr, arms, source) => Self::Match(
                Box::new(expr.hir_into(hir)),
                arms.into_iter().map(|a| a.hir_into(hir)).collect(),
                source.into(),
            ),
            ExprKind::Closure(c) => Self::Closure(c.hir_into(hir)),
            ExprKind::Block(b, _) => Self::Block(b.hir_into(hir)),
            ExprKind::Field(expr, field) => Self::Field(Box::new(expr.hir_into(hir)), field.into()),
            ExprKind::Index(expr, idx) => {
                Self::Index(Box::new(expr.hir_into(hir)), Box::new(idx.hir_into(hir)))
            }
            ExprKind::Path(p) => Self::Path(p.into()),
            ExprKind::AddrOf(brw, m, expr) => Self::AddrOf(brw, m, Box::new(expr.hir_into(hir))),
            ExprKind::Struct(p, fields, rest) => Self::Struct(
                p.into(),
                fields.into_iter().map(|f| f.hir_into(hir)).collect(),
                rest.map(|r| Box::new(r.hir_into(hir))),
            ),
            ExprKind::Repeat(expr, len) => Self::Repeat(Box::new(expr.hir_into(hir)), len),
            k => panic!("Unsupported kind {k:?}"),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Let<'hir>> for TermLet {
    fn from_hir(value: &'hir Let<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            pat: value.pat.into(),
            ty: value.ty.map(|ty| ty.into()),
            init: Box::new(value.init.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Arm<'hir>> for TermArm {
    fn from_hir(value: &'hir Arm<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            pat: value.pat.into(),
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
            bound_generic_params: value.bound_generic_params.into(),
            fn_decl: value.fn_decl.into(),
            body: hir.body(value.body).hir_into(hir),
            fn_decl_span: value.fn_decl_span.into(),
            fn_arg_span: value.fn_arg_span.map(|s| s.into()),
            movability: value.movability.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Block<'hir>> for TermBlock {
    fn from_hir(value: &'hir Block<'hir>, hir: Map<'hir>) -> Self {
        Self {
            stmts: value.stmts.into_iter().map(|s| s.hir_into(hir)).collect(),
            term: value.expr.map(|e| Box::new(e.hir_into(hir))),
            hir_id: value.hir_id.into(),
            rules: value.rules,
            span: value.span.into(),
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

impl<'hir> FromHir<'hir, Guard<'hir>> for TermGuard {
    fn from_hir(value: Guard<'hir>, hir: Map<'hir>) -> Self {
        match value {
            Guard::If(e) => Self::If(Box::new(e.hir_into(hir))),
            Guard::IfLet(l) => Self::IfLet(l.hir_into(hir)),
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
            StmtKind::Local(l) => Self::Local(l.hir_into(hir)),
            StmtKind::Item(i) => Self::Item(i),
            StmtKind::Expr(e) => Self::Term(Box::new(e.hir_into(hir))),
            StmtKind::Semi(e) => Self::Semi(Box::new(e.hir_into(hir))),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Local<'hir>> for TermLocal {
    fn from_hir(value: &'hir Local<'hir>, hir: Map<'hir>) -> Self {
        Self {
            pat: value.pat,
            ty: value.ty.into(),
            init: value.init.map(|e| Box::new(e.hir_into(hir))),
            hir_id: value.hir_id.into(),
            span: value.span.into(),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Body<'hir>> for TermBody {
    fn from_hir(value: &'hir Body<'hir>, hir: Map<'hir>) -> Self {
        Self {
            params: value.params,
            value: Box::new(value.value.hir_into(hir)),
        }
    }
}

impl<'hir> From<&'hir PathSegment<'hir>> for TermPathSegment {
    fn from(value: &PathSegment) -> Self {
        Self {
            ident: value.ident.into(),
            hir_id: value.hir_id.into(),
            res: value.res.into(),
            args: value.args.into_iter().map(|a| a.into()).collect(),
            infer_args: value.infer_args,
        }
    }
}

impl<'hir> From<&'hir Ty<'hir>> for TermTy {
    fn from(value: &'hir Ty<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            kind: value.kind.into(),
            span: value.span.into(),
        }
    }
}

impl<'hir> From<TyKind<'hir>> for TermTyKind {
    fn from(value: TyKind<'hir>) -> Self {
        match value {
            TyKind::Slice(ty) => Self::Slice(Box::new(ty.into())),
            TyKind::Array(_, _) => todo!(),
            TyKind::Ptr(_) => todo!(),
            TyKind::Ref(_, _) => todo!(),
            TyKind::BareFn(_) => todo!(),
            TyKind::Never => todo!(),
            TyKind::Tup(_) => todo!(),
            TyKind::Path(_) => todo!(),
            TyKind::OpaqueDef(_, _, _) => todo!(),
            TyKind::TraitObject(_, _, _) => todo!(),
            TyKind::Typeof(_) => todo!(),
            TyKind::Infer => todo!(),
            TyKind::Err(_) => todo!(),
        }
    }
}

impl From<BinOp> for TermBinOp {
    fn from(value: BinOp) -> Self {
        todo!()
    }
}

impl From<UnOp> for TermUnOp {
    fn from(value: UnOp) -> Self {
        todo!()
    }
}

impl<'hir> From<&'hir Lit> for TermLit {
    fn from(value: &'hir Lit) -> Self {
        todo!()
    }
}

impl From<MatchSource> for TermMatchSource {
    fn from(value: MatchSource) -> Self {
        todo!()
    }
}

impl<'hir> From<QPath<'hir>> for TermQPath {
    fn from(value: QPath<'hir>) -> Self {
        todo!()
    }
}

impl<'hir> From<&'hir QPath<'hir>> for TermQPath {
    fn from(value: &'hir QPath<'hir>) -> Self {
        todo!()
    }
}

enum StubKind {
    Exists,
    Forall,
}

fn is_stub(segments: &[PathSegment]) -> bool {
    if segments.len() != 4 {
        return false;
    }

    if segments[0].ident.name == Symbol::intern("{{root}}")
        && segments[1].ident.name == Symbol::intern("rml_contracts")
        && segments[2].ident.name == Symbol::intern("stubs")
    {
        true
    } else {
        false
    }
}

fn get_stub_kind(segments: &[PathSegment]) -> Option<StubKind> {
    let name = segments[3].ident.name;
    if name == Symbol::intern("exists") {
        Some(StubKind::Exists)
    } else if name == Symbol::intern("forall") {
        Some(StubKind::Forall)
    } else {
        None
    }
}
