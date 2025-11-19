use paste::paste;

use super::*;

macro_rules! create_visitor_traits {
    ($($name:ident : $ty:ty), *) => {
        /// Syntax tree traversal to walk a shared borrow of a [Term] syntax tree.
        pub trait Visit<'ast> {
            $(
                paste!{
                    fn [< visit_ $name >] (&mut self, t: &'ast $ty) {
                        [< visit_ $name >](self, t)
                    }
                }
            )*
        }

        $(
            impl Visitable for $ty {
                fn visit<'ast, V: Visit<'ast>>(&'ast self, v: &mut V) {
                    paste!{
                        v.[< visit_ $name >](self)
                    }
                }
            }
        )*
    };
}

/// Conveniance trait for traversal of a shared borrow of a syntax tree.
pub trait Visitable {
    fn visit<'ast, V: Visit<'ast>>(&'ast self, v: &mut V);
}

// Only necessary fns for collecting types rn.
create_visitor_traits! {
  mod: Mod,
  item: Item,
  item_kind: ItemKind,
  body: Body,
  impl: Impl,
  expr: Expr,
  const_block: ConstBlock,
  let_expr: LetExpr,
  block: Block,
  arm: Arm,
  closure: Closure,
  expr_field: ExprField,
  stmt: Stmt,
  let_stmt: LetStmt,
  param: Param,
  pat: Pat,
  pat_expr: PatExpr,
  const_arg: ConstArg,
  anon_const: AnonConst
}

pub fn visit_mod<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Mod) {
    for i in &x.items {
        v.visit_item(i);
    }
}

pub fn visit_item<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Item) {
    v.visit_item_kind(&x.kind);
}

pub fn visit_item_kind<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a ItemKind) {
    match x {
        ItemKind::ExternCrate {
            symbol: _,
            ident: _,
        } => {}
        ItemKind::Use { .. } => {}
        ItemKind::Static { body, .. } => {
            v.visit_body(body);
        }
        ItemKind::Const { body, .. } => {
            v.visit_body(body);
        }
        ItemKind::Fn { body, .. } => {
            v.visit_body(body);
        }
        ItemKind::Mod { r#mod: m, .. } => v.visit_mod(m),
        ItemKind::TyAlias { .. } => {}
        ItemKind::Enum { .. } => {}
        ItemKind::Struct { .. } => {}
        ItemKind::Union { .. } => {}
        ItemKind::Trait { .. } => {}
        ItemKind::TraitAlias { .. } => {}
        ItemKind::Impl { r#impl: i } => v.visit_impl(i),
        ItemKind::Macro { .. } => {}
        ItemKind::ForeignMod => {}
        ItemKind::GlobalAsm => {}
    }
}

pub fn visit_body<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Body) {
    for p in &x.params {
        v.visit_param(p);
    }
    v.visit_expr(&x.value);
}

pub fn visit_param<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Param) {
    v.visit_pat(&x.pat);
}

pub fn visit_pat<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Pat) {
    match x.kind.as_ref() {
        PatKind::Wild => {}
        PatKind::Binding { pat, .. } => {
            if let Some(p) = pat {
                v.visit_pat(p);
            }
        }
        PatKind::Struct { .. } => {}
        PatKind::TupleStruct { pats, .. } => {
            for p in pats {
                v.visit_pat(p);
            }
        }
        PatKind::Or { pats } => {
            for p in pats {
                v.visit_pat(p);
            }
        }
        PatKind::Never => {}
        PatKind::Path { .. } => {}
        PatKind::Tuple { pats, .. } => {
            for p in pats {
                v.visit_pat(p);
            }
        }
        PatKind::Box { pat } => v.visit_pat(pat),
        PatKind::Deref { pat } => v.visit_pat(pat),
        PatKind::Ref { pat, .. } => v.visit_pat(pat),
        PatKind::Lit { expr } => v.visit_expr(expr),
        PatKind::Range { lhs, rhs, .. } => {
            if let Some(e) = lhs {
                v.visit_pat_expr(e);
            }
            if let Some(e) = rhs {
                v.visit_pat_expr(e);
            }
        }
        PatKind::Slice { start, mid, rest } => {
            for p in start {
                v.visit_pat(p);
            }
            if let Some(p) = mid {
                v.visit_pat(p);
            }
            for p in rest {
                v.visit_pat(p);
            }
        }
        PatKind::Err => {}
        PatKind::Missing => todo!(),
        PatKind::Expr { expr } => v.visit_pat_expr(expr),
        PatKind::Guard { pat, guard } => {
            v.visit_pat(pat);
            v.visit_expr(guard);
        }
    }
}

pub fn visit_impl<'a, V: Visit<'a> + ?Sized>(_v: &mut V, _x: &'a Impl) {}

pub fn visit_expr<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Expr) {
    match x.kind.as_ref() {
        ExprKind::ConstBlock { block } => v.visit_const_block(block),
        ExprKind::Array { exprs } => {
            for e in exprs {
                v.visit_expr(e);
            }
        }
        ExprKind::Call { callee, args } => {
            v.visit_expr(callee);
            for e in args {
                v.visit_expr(e);
            }
        }
        ExprKind::MethodCall { callee, args, .. } => {
            v.visit_expr(callee);
            for e in args {
                v.visit_expr(e);
            }
        }
        ExprKind::Tup { exprs } => {
            for e in exprs {
                v.visit_expr(e);
            }
        }
        ExprKind::Binary { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }
        ExprKind::Unary { expr, .. } => {
            v.visit_expr(expr);
        }
        ExprKind::Lit { .. } => {}
        ExprKind::Cast { expr, .. } => {
            v.visit_expr(expr);
        }
        ExprKind::Type { expr, .. } => {
            v.visit_expr(expr);
        }
        ExprKind::DropTemps { expr } => {
            v.visit_expr(expr);
        }
        ExprKind::Let { r#let } => {
            v.visit_let_expr(r#let);
        }
        ExprKind::If { cond, then, els } => {
            v.visit_expr(cond);
            v.visit_expr(then);
            if let Some(e) = els {
                v.visit_expr(e);
            }
        }
        ExprKind::Loop { block, .. } => {
            v.visit_block(block);
        }
        ExprKind::Match { expr, arms, .. } => {
            v.visit_expr(expr);
            for a in arms {
                v.visit_arm(a);
            }
        }
        ExprKind::Closure { closure } => v.visit_closure(closure),
        ExprKind::Block { block, .. } => {
            v.visit_block(block);
        }
        ExprKind::GhostBlock { block } => {
            v.visit_block(block);
        }
        ExprKind::Assign { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }
        ExprKind::AssignOp { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }
        ExprKind::Field { expr, .. } => {
            v.visit_expr(expr);
        }
        ExprKind::Index { base, idx, .. } => {
            v.visit_expr(base);
            v.visit_expr(idx);
        }
        ExprKind::Path { .. } => {}
        ExprKind::AddrOf { expr, .. } => {
            v.visit_expr(expr);
        }
        ExprKind::Break { expr, .. } => {
            if let Some(e) = expr {
                v.visit_expr(e);
            }
        }
        ExprKind::Continue { .. } => {}
        ExprKind::Ret { expr } => {
            if let Some(e) = expr {
                v.visit_expr(e);
            }
        }
        ExprKind::Become { expr } => v.visit_expr(expr),
        ExprKind::InlineAsm => {}
        ExprKind::OffsetOf { .. } => {}
        ExprKind::Struct {
            fields, tail: rest, ..
        } => {
            for f in fields {
                v.visit_expr_field(f);
            }
            if let StructTailExpr::Base { base } = rest {
                v.visit_expr(base);
            }
        }
        ExprKind::Repeat { expr, len } => {
            v.visit_expr(expr);
            v.visit_const_arg(len);
        }
        ExprKind::Yield { expr, .. } => {
            v.visit_expr(expr);
        }
        ExprKind::Err => {}
        ExprKind::Use { .. } => todo!(),
    }
}

pub fn visit_const_block<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a ConstBlock) {
    v.visit_body(&x.body);
}

pub fn visit_let_expr<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a LetExpr) {
    v.visit_pat(&x.pat);
    v.visit_expr(&x.init);
}

pub fn visit_block<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Block) {
    for s in &x.stmts {
        v.visit_stmt(s);
    }
    if let Some(e) = &x.expr {
        v.visit_expr(e);
    }
}

pub fn visit_arm<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Arm) {
    v.visit_pat(&x.pat);
    if let Some(e) = &x.guard {
        v.visit_expr(e);
    }
    v.visit_expr(&x.body);
}

pub fn visit_closure<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Closure) {
    v.visit_body(&x.body);
}

pub fn visit_expr_field<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a ExprField) {
    v.visit_expr(&x.expr);
}

pub fn visit_stmt<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Stmt) {
    match &x.kind {
        StmtKind::Let { r#let } => v.visit_let_stmt(r#let),
        StmtKind::Item { item } => v.visit_item(item),
        StmtKind::Expr { expr } => v.visit_expr(expr),
        StmtKind::Semi { expr } => v.visit_expr(expr),
    }
}

pub fn visit_let_stmt<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a LetStmt) {
    v.visit_pat(&x.pat);
    if let Some(e) = &x.init {
        v.visit_expr(e);
    }
}

pub fn visit_pat_expr<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a PatExpr) {
    match &x.kind {
        PatExprKind::Lit { .. } => {}
        PatExprKind::ConstBlock { block: cb } => v.visit_body(&cb.body),
        PatExprKind::Path { .. } => {}
    }
}

pub fn visit_const_arg<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a ConstArg) {
    match &x.kind {
        ConstArgKind::Path { .. } => {}
        ConstArgKind::Anon { ac } => v.visit_anon_const(ac),
        ConstArgKind::Infer { .. } => {}
    }
}

pub fn visit_anon_const<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a AnonConst) {
    v.visit_body(&x.body);
}
