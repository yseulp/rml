use rustc_ast::Label;
use rustc_hir::{
    def_id::LocalDefId, Arm, ArrayLen, BinOp, Block, BlockCheckMode, Body, BorrowKind, CaptureBy,
    Closure, ClosureBinder, Constness, Expr, ExprField, ExprKind, FnDecl, GenericParam, Guard,
    HirId, ItemId, Let, Lit, Local, MatchSource, Movability, Mutability, Param, Pat, PatKind, Path,
    PathSegment, QPath, Stmt, StmtKind, Ty, UnOp,
};
use rustc_middle::hir::map::Map;
use rustc_span::{symbol::Ident, Span, Symbol};

#[derive(Debug, Clone)]
pub struct Term<'hir> {
    pub hir_id: HirId,
    pub kind: TermKind<'hir>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TermKind<'hir> {
    Array(Vec<Term<'hir>>),
    Call(Box<Term<'hir>>, Vec<Term<'hir>>),
    MethodCall(
        &'hir PathSegment<'hir>,
        Box<Term<'hir>>,
        Vec<Term<'hir>>,
        Span,
    ),
    Tup(Vec<Term<'hir>>),
    Binary(BinOp, Box<Term<'hir>>, Box<Term<'hir>>),
    Unary(UnOp, Box<Term<'hir>>),
    Lit(&'hir Lit),
    Cast(Box<Term<'hir>>, &'hir Ty<'hir>),
    Let(TermLet<'hir>),
    If(Box<Term<'hir>>, Box<Term<'hir>>, Option<Box<Term<'hir>>>),
    Match(Box<Term<'hir>>, Vec<TermArm<'hir>>, MatchSource),
    Closure(TermClosure<'hir>),
    Block(TermBlock<'hir>, Option<Label>),
    Field(Box<Term<'hir>>, Ident),
    Index(Box<Term<'hir>>, Box<Term<'hir>>),
    Path(QPath<'hir>),
    AddrOf(BorrowKind, Mutability, Box<Term<'hir>>),
    Struct(
        &'hir QPath<'hir>,
        Vec<TermField<'hir>>,
        Option<Box<Term<'hir>>>,
    ),
    Repeat(Box<Term<'hir>>, ArrayLen),

    // RML special kinds
    Quantor(QuantorKind, QuantorParam<'hir>, Box<Term<'hir>>),
    LogEq(Box<Term<'hir>>, Box<Term<'hir>>),
}

#[derive(Debug, Clone)]
pub struct TermLet<'hir> {
    pub hir_id: HirId,
    pub span: Span,
    pub pat: &'hir Pat<'hir>,
    pub ty: Option<&'hir Ty<'hir>>,
    pub init: Box<Term<'hir>>,
}

#[derive(Debug, Clone)]
pub struct TermArm<'hir> {
    pub hir_id: HirId,
    pub span: Span,
    pub pat: &'hir Pat<'hir>,
    pub guard: Option<TermGuard<'hir>>,
    pub body: Box<Term<'hir>>,
}

#[derive(Debug, Clone)]
pub struct TermClosure<'hir> {
    pub def_id: LocalDefId,
    pub binder: ClosureBinder,
    pub constness: Constness,
    pub capture_clause: CaptureBy,
    pub bound_generic_params: &'hir [GenericParam<'hir>],
    pub fn_decl: &'hir FnDecl<'hir>,
    pub body: TermBody<'hir>,
    pub fn_decl_span: Span,
    pub fn_arg_span: Option<Span>,
    pub movability: Option<Movability>,
}

#[derive(Debug, Clone)]
pub struct TermBlock<'hir> {
    pub stmts: Vec<TermStmt<'hir>>,
    pub term: Option<Box<Term<'hir>>>,
    pub hir_id: HirId,
    pub rules: BlockCheckMode,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TermGuard<'hir> {
    If(Box<Term<'hir>>),
    IfLet(TermLet<'hir>),
}

#[derive(Debug, Clone)]
pub struct TermStmt<'hir> {
    pub hir_id: HirId,
    pub kind: TermStmtKind<'hir>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TermStmtKind<'hir> {
    Local(TermLocal<'hir>),
    Item(ItemId),
    Term(Box<Term<'hir>>),
    Semi(Box<Term<'hir>>),
}

#[derive(Debug, Clone)]
pub struct TermLocal<'hir> {
    pub pat: &'hir Pat<'hir>,
    pub ty: Option<&'hir Ty<'hir>>,
    pub init: Option<Box<Term<'hir>>>,
    pub hir_id: HirId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TermField<'hir> {
    pub hir_id: HirId,
    pub ident: Ident,
    pub term: Term<'hir>,
    pub span: Span,
    pub is_shorthand: bool,
}

#[derive(Debug, Clone)]
pub struct TermBody<'hir> {
    pub params: &'hir [Param<'hir>],
    pub value: Box<Term<'hir>>,
}

#[derive(Debug, Clone, Copy)]
pub enum QuantorKind {
    Exists,
    Forall,
}

#[derive(Debug, Clone)]
pub struct QuantorParam<'hir> {
    pub hir_id: HirId,
    pub ident: Ident,
    pub ty: &'hir Ty<'hir>,
    pub span: Span,
    pub ty_span: Span,
}

enum StubKind {
    Exists,
    Forall,
}

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

impl<'hir> FromHir<'hir, &'hir Expr<'hir>> for Term<'hir> {
    fn from_hir(value: &'hir Expr<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id,
            span: value.span,
            kind: value.kind.hir_into(hir),
        }
    }
}

impl<'hir> FromHir<'hir, ExprKind<'hir>> for TermKind<'hir> {
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
                            PatKind::Binding(_, _, ident, _) => ident,
                            _ => unreachable!(),
                        };

                        QuantorParam {
                            hir_id: c_param.hir_id,
                            ident,
                            ty: &clo.fn_decl.inputs[0],
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
                path,
                Box::new(recv.hir_into(hir)),
                args.into_iter().map(|a| a.hir_into(hir)).collect(),
                span,
            ),
            ExprKind::Tup(exprs) => Self::Tup(exprs.into_iter().map(|e| e.hir_into(hir)).collect()),
            ExprKind::Binary(op, left, right) => Self::Binary(
                op,
                Box::new(left.hir_into(hir)),
                Box::new(right.hir_into(hir)),
            ),
            ExprKind::Unary(op, expr) => Self::Unary(op, Box::new(expr.hir_into(hir))),
            ExprKind::Lit(lit) => Self::Lit(lit),
            ExprKind::Cast(expr, ty) => Self::Cast(Box::new(expr.hir_into(hir)), ty),
            ExprKind::Let(l) => Self::Let(l.hir_into(hir)),
            ExprKind::If(cond, then, els) => Self::If(
                Box::new(cond.hir_into(hir)),
                Box::new(then.hir_into(hir)),
                els.map(|e| Box::new(e.hir_into(hir))),
            ),
            ExprKind::Match(expr, arms, source) => Self::Match(
                Box::new(expr.hir_into(hir)),
                arms.into_iter().map(|a| a.hir_into(hir)).collect(),
                source,
            ),
            ExprKind::Closure(c) => Self::Closure(c.hir_into(hir)),
            ExprKind::Block(b, l) => Self::Block(b.hir_into(hir), l),
            ExprKind::Field(expr, field) => Self::Field(Box::new(expr.hir_into(hir)), field),
            ExprKind::Index(expr, idx) => {
                Self::Index(Box::new(expr.hir_into(hir)), Box::new(idx.hir_into(hir)))
            }
            ExprKind::Path(p) => Self::Path(p),
            ExprKind::AddrOf(brw, m, expr) => Self::AddrOf(brw, m, Box::new(expr.hir_into(hir))),
            ExprKind::Struct(p, fields, rest) => Self::Struct(
                p,
                fields.into_iter().map(|f| f.hir_into(hir)).collect(),
                rest.map(|r| Box::new(r.hir_into(hir))),
            ),
            ExprKind::Repeat(expr, len) => Self::Repeat(Box::new(expr.hir_into(hir)), len),
            k => panic!("Unsupported kind {k:?}"),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Let<'hir>> for TermLet<'hir> {
    fn from_hir(value: &'hir Let<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id,
            span: value.span,
            pat: value.pat,
            ty: value.ty,
            init: Box::new(value.init.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Arm<'hir>> for TermArm<'hir> {
    fn from_hir(value: &'hir Arm<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id,
            span: value.span,
            pat: value.pat,
            guard: value.guard.map(|g| g.hir_into(hir)),
            body: Box::new(value.body.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Closure<'hir>> for TermClosure<'hir> {
    fn from_hir(value: &'hir Closure<'hir>, hir: Map<'hir>) -> Self {
        Self {
            def_id: value.def_id,
            binder: value.binder,
            constness: value.constness,
            capture_clause: value.capture_clause,
            bound_generic_params: value.bound_generic_params,
            fn_decl: value.fn_decl,
            body: hir.body(value.body).hir_into(hir),
            fn_decl_span: value.fn_decl_span,
            fn_arg_span: value.fn_arg_span,
            movability: value.movability,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Block<'hir>> for TermBlock<'hir> {
    fn from_hir(value: &'hir Block<'hir>, hir: Map<'hir>) -> Self {
        Self {
            stmts: value.stmts.into_iter().map(|s| s.hir_into(hir)).collect(),
            term: value.expr.map(|e| Box::new(e.hir_into(hir))),
            hir_id: value.hir_id,
            rules: value.rules,
            span: value.span,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir ExprField<'hir>> for TermField<'hir> {
    fn from_hir(value: &'hir ExprField<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id,
            ident: value.ident,
            term: value.expr.hir_into(hir),
            span: value.span,
            is_shorthand: value.is_shorthand,
        }
    }
}

impl<'hir> FromHir<'hir, Guard<'hir>> for TermGuard<'hir> {
    fn from_hir(value: Guard<'hir>, hir: Map<'hir>) -> Self {
        match value {
            Guard::If(e) => Self::If(Box::new(e.hir_into(hir))),
            Guard::IfLet(l) => Self::IfLet(l.hir_into(hir)),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Stmt<'hir>> for TermStmt<'hir> {
    fn from_hir(value: &'hir Stmt<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id,
            kind: value.kind.hir_into(hir),
            span: value.span,
        }
    }
}

impl<'hir> FromHir<'hir, StmtKind<'hir>> for TermStmtKind<'hir> {
    fn from_hir(value: StmtKind<'hir>, hir: Map<'hir>) -> Self {
        match value {
            StmtKind::Local(l) => Self::Local(l.hir_into(hir)),
            StmtKind::Item(i) => Self::Item(i),
            StmtKind::Expr(e) => Self::Term(Box::new(e.hir_into(hir))),
            StmtKind::Semi(e) => Self::Semi(Box::new(e.hir_into(hir))),
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Local<'hir>> for TermLocal<'hir> {
    fn from_hir(value: &'hir Local<'hir>, hir: Map<'hir>) -> Self {
        Self {
            pat: value.pat,
            ty: value.ty,
            init: value.init.map(|e| Box::new(e.hir_into(hir))),
            hir_id: value.hir_id,
            span: value.span,
        }
    }
}

impl<'hir> FromHir<'hir, &'hir Body<'hir>> for TermBody<'hir> {
    fn from_hir(value: &'hir Body<'hir>, hir: Map<'hir>) -> Self {
        Self {
            params: value.params,
            value: Box::new(value.value.hir_into(hir)),
        }
    }
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
