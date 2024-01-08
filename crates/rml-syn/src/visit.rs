//! Syntax tree traversal to walk a shared or mutable borrow of a syntax tree of
//! term data structures. Similar to [syn::visit] and [syn::visit_mut].
//!
//! Each method of the [Visit] and [VisitMut] traits is a hook that can be
//! overridden to customize the behavior when visiting the corresponding type of
//! node. By default, every method recursively visits the substructure of the
//! input by invoking the right visitor method of each of its fields.

use paste::paste;

use crate::{
    FieldPat, LocSet, LocSetField, LocSetFieldWildcard, LocSetGroup, LocSetIndex, LocSetNothing,
    LocSetPath, Pat, PatIdent, PatOr, PatParen, PatRest, PatSlice, PatStruct, PatTuple,
    PatTupleStruct, PatType, PatWild, QuantArg, Spec, TBlock, TLocal, Term,
    TermAngleBracketedGenericArguments, TermArm, TermArray, TermBinary, TermBlock, TermCall,
    TermCast, TermClosure, TermExists, TermField, TermFieldValue, TermFinal, TermForall,
    TermGenericMethodArgument, TermGroup, TermIf, TermImpl, TermIndex, TermLet, TermLit, TermLogEq,
    TermMatch, TermMethodCall, TermModel, TermOld, TermParen, TermPath, TermRange, TermRepeat,
    TermStmt, TermStruct, TermTuple, TermUnary,
};

macro_rules! create_visitor_traits {
    ($($name:ident : $ty:ty), *) => {

        /// Syntax tree traversal to walk a shared borrow of a [Term] syntax tree.
        pub trait Visit<'ast>: syn::visit::Visit<'ast> {
            $(
                paste!{
                    fn [< visit_ $name >] (&mut self, t: &'ast $ty) {
                        [< visit_ $name >](self, t)
                    }
                }
            )*
        }

        /// Syntax tree traversal to walk a mutable borrow of a [Term] syntax tree.
        pub trait VisitMut: syn::visit_mut::VisitMut {
            $(
                paste!{
                    fn [< visit_ $name _mut >] (&mut self, t: &mut $ty) {
                        [< visit_ $name _mut >](self, t)
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

        $(
            impl MutVisitable for $ty {
                fn visit<V: VisitMut>(&mut self, v: &mut V) {
                    paste!{
                        v.[< visit_ $name _mut >](self)
                    }
                }
            }
        )*
    };
}

create_visitor_traits! {
    term: Term,
    term_array: TermArray,
    term_binary: TermBinary,
    term_block: TermBlock,
    term_call: TermCall,
    term_cast: TermCast,
    term_closure: TermClosure,
    term_exists: TermExists,
    term_field: TermField,
    term_final: TermFinal,
    term_forall: TermForall,
    term_group: TermGroup,
    term_if: TermIf,
    term_impl: TermImpl,
    term_index: TermIndex,
    term_let: TermLet,
    term_lit: TermLit,
    term_log_eq: TermLogEq,
    term_match: TermMatch,
    term_method_call: TermMethodCall,
    term_model: TermModel,
    term_old: TermOld,
    term_paren: TermParen,
    term_path: TermPath,
    term_range: TermRange,
    term_repeat: TermRepeat,
    term_struct: TermStruct,
    term_tuple: TermTuple,
    term_unary: TermUnary,

    tblock: TBlock,
    term_stmt: TermStmt,
    tlocal: TLocal,
    term_arm: TermArm,
    term_field_value: TermFieldValue,
    term_angle_bracketed_generic_arguments: TermAngleBracketedGenericArguments,
    term_generic_method_argument: TermGenericMethodArgument,
    quant_arg: QuantArg,

    tpat: Pat,
    tpat_ident: PatIdent,
    tpat_or: PatOr,
    tpat_paren: PatParen,
    tpat_rest: PatRest,
    tpat_slice: PatSlice,
    tpat_struct: PatStruct,
    tpat_tuple: PatTuple,
    tpat_tuple_struct: PatTupleStruct,
    tpat_type: PatType,
    tpat_wild: PatWild,

    tfield_pat: FieldPat,

    loc_set: LocSet,
    loc_set_field: LocSetField,
    loc_set_field_wildcard: LocSetFieldWildcard,
    loc_set_index: LocSetIndex,
    loc_set_path: LocSetPath,
    loc_set_group: LocSetGroup,
    loc_set_nothing: LocSetNothing,

    spec: Spec
}

/// Conveniance trait for traversal of a shared borrow of a syntax tree.
pub trait Visitable {
    fn visit<'ast, V: Visit<'ast>>(&'ast self, v: &mut V);
}

/// Conveniance trait for traversal of a mutable borrow of a syntax tree.
pub trait MutVisitable {
    fn visit<V: VisitMut>(&mut self, v: &mut V);
}

pub fn visit_term<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast Term) {
    match t {
        Term::Array(t) => v.visit_term_array(t),
        Term::Binary(t) => v.visit_term_binary(t),
        Term::Block(t) => v.visit_term_block(t),
        Term::Call(t) => v.visit_term_call(t),
        Term::Cast(t) => v.visit_term_cast(t),
        Term::Closure(t) => v.visit_term_closure(t),
        Term::Exists(t) => v.visit_term_exists(t),
        Term::Field(t) => v.visit_term_field(t),
        Term::Final(t) => v.visit_term_final(t),
        Term::Forall(t) => v.visit_term_forall(t),
        Term::Group(t) => v.visit_term_group(t),
        Term::If(t) => v.visit_term_if(t),
        Term::Impl(t) => v.visit_term_impl(t),
        Term::Index(t) => v.visit_term_index(t),
        Term::Let(t) => v.visit_term_let(t),
        Term::Lit(t) => v.visit_term_lit(t),
        Term::LogEq(t) => v.visit_term_log_eq(t),
        Term::Macro(t) => v.visit_expr_macro(t),
        Term::Match(t) => v.visit_term_match(t),
        Term::MethodCall(t) => v.visit_term_method_call(t),
        Term::Model(t) => v.visit_term_model(t),
        Term::Old(t) => v.visit_term_old(t),
        Term::Paren(t) => v.visit_term_paren(t),
        Term::Path(t) => v.visit_term_path(t),
        Term::Range(t) => v.visit_term_range(t),
        Term::Repeat(t) => v.visit_term_repeat(t),
        Term::Struct(t) => v.visit_term_struct(t),
        Term::Tuple(t) => v.visit_term_tuple(t),
        Term::Unary(t) => v.visit_term_unary(t),
        Term::Verbatim(_) => todo!(),
    }
}
pub fn visit_term_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut Term) {
    match t {
        Term::Array(t) => v.visit_term_array_mut(t),
        Term::Binary(t) => v.visit_term_binary_mut(t),
        Term::Block(t) => v.visit_term_block_mut(t),
        Term::Call(t) => v.visit_term_call_mut(t),
        Term::Cast(t) => v.visit_term_cast_mut(t),
        Term::Closure(t) => v.visit_term_closure_mut(t),
        Term::Exists(t) => v.visit_term_exists_mut(t),
        Term::Field(t) => v.visit_term_field_mut(t),
        Term::Final(t) => v.visit_term_final_mut(t),
        Term::Forall(t) => v.visit_term_forall_mut(t),
        Term::Group(t) => v.visit_term_group_mut(t),
        Term::If(t) => v.visit_term_if_mut(t),
        Term::Impl(t) => v.visit_term_impl_mut(t),
        Term::Index(t) => v.visit_term_index_mut(t),
        Term::Let(t) => v.visit_term_let_mut(t),
        Term::Lit(t) => v.visit_term_lit_mut(t),
        Term::LogEq(t) => v.visit_term_log_eq_mut(t),
        Term::Macro(t) => v.visit_expr_macro_mut(t),
        Term::Match(t) => v.visit_term_match_mut(t),
        Term::MethodCall(t) => v.visit_term_method_call_mut(t),
        Term::Model(t) => v.visit_term_model_mut(t),
        Term::Old(t) => v.visit_term_old_mut(t),
        Term::Paren(t) => v.visit_term_paren_mut(t),
        Term::Path(t) => v.visit_term_path_mut(t),
        Term::Range(t) => v.visit_term_range_mut(t),
        Term::Repeat(t) => v.visit_term_repeat_mut(t),
        Term::Struct(t) => v.visit_term_struct_mut(t),
        Term::Tuple(t) => v.visit_term_tuple_mut(t),
        Term::Unary(t) => v.visit_term_unary_mut(t),
        Term::Verbatim(_) => todo!(),
    }
}

pub fn visit_term_array<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermArray) {
    for e in &t.elems {
        v.visit_term(e)
    }
}
pub fn visit_term_array_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermArray) {
    for e in &mut t.elems {
        v.visit_term_mut(e)
    }
}

pub fn visit_term_binary<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermBinary) {
    v.visit_term(&t.left);
    v.visit_bin_op(&t.op);
    v.visit_term(&t.right);
}
pub fn visit_term_binary_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermBinary) {
    v.visit_term_mut(&mut t.left);
    v.visit_bin_op_mut(&mut t.op);
    v.visit_term_mut(&mut t.right);
}

pub fn visit_term_block<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermBlock) {
    if let Some(l) = &t.label {
        v.visit_label(l);
    }
    v.visit_tblock(&t.block);
}
pub fn visit_term_block_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermBlock) {
    if let Some(l) = &mut t.label {
        v.visit_label_mut(l);
    }
    v.visit_tblock_mut(&mut t.block);
}

pub fn visit_term_call<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermCall) {
    v.visit_term(&t.func);
    for a in &t.args {
        v.visit_term(a);
    }
}
pub fn visit_term_call_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermCall) {
    v.visit_term_mut(&mut t.func);
    for a in &mut t.args {
        v.visit_term_mut(a);
    }
}

pub fn visit_term_cast<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermCast) {
    v.visit_term(&t.term);
    v.visit_type(&t.ty);
}
pub fn visit_term_cast_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermCast) {
    v.visit_term_mut(&mut t.term);
    v.visit_type_mut(&mut t.ty);
}

pub fn visit_term_closure<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermClosure) {
    for p in &t.inputs {
        v.visit_tpat(p);
    }
    v.visit_return_type(&t.output);
    v.visit_term(&t.body);
}
pub fn visit_term_closure_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermClosure) {
    for p in &mut t.inputs {
        v.visit_tpat_mut(p);
    }
    v.visit_return_type_mut(&mut t.output);
    v.visit_term_mut(&mut t.body);
}

pub fn visit_term_exists<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermExists) {
    for a in &t.args {
        v.visit_quant_arg(a);
    }
    v.visit_term(&t.term);
}
pub fn visit_term_exists_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermExists) {
    for a in &mut t.args {
        v.visit_quant_arg_mut(a);
    }
    v.visit_term_mut(&mut t.term);
}

pub fn visit_term_field<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermField) {
    v.visit_term(&t.base);
    v.visit_member(&t.member);
}
pub fn visit_term_field_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermField) {
    v.visit_term_mut(&mut t.base);
    v.visit_member_mut(&mut t.member);
}

pub fn visit_term_final<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermFinal) {
    v.visit_term(&t.term);
}
pub fn visit_term_final_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermFinal) {
    v.visit_term_mut(&mut t.term);
}

pub fn visit_term_forall<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermForall) {
    for a in &t.args {
        v.visit_quant_arg(a);
    }
    v.visit_term(&t.term);
}
pub fn visit_term_forall_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermForall) {
    for a in &mut t.args {
        v.visit_quant_arg_mut(a);
    }
    v.visit_term_mut(&mut t.term);
}

pub fn visit_term_group<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermGroup) {
    v.visit_term(&t.term);
}
pub fn visit_term_group_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermGroup) {
    v.visit_term_mut(&mut t.term);
}

pub fn visit_term_if<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermIf) {
    v.visit_term(&t.cond);
    v.visit_tblock(&t.then_branch);
    if let Some(e) = &t.else_branch {
        v.visit_term(&e.1);
    }
}
pub fn visit_term_if_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermIf) {
    v.visit_term_mut(&mut t.cond);
    v.visit_tblock_mut(&mut t.then_branch);
    if let Some(e) = &mut t.else_branch {
        v.visit_term_mut(&mut e.1);
    }
}

pub fn visit_term_impl<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermImpl) {
    v.visit_term(&t.hyp);
    v.visit_term(&t.cons);
}
pub fn visit_term_impl_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermImpl) {
    v.visit_term_mut(&mut t.hyp);
    v.visit_term_mut(&mut t.cons);
}

pub fn visit_term_index<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermIndex) {
    v.visit_term(&t.term);
    v.visit_term(&t.index);
}
pub fn visit_term_index_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermIndex) {
    v.visit_term_mut(&mut t.term);
    v.visit_term_mut(&mut t.index);
}

pub fn visit_term_let<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermLet) {
    v.visit_tpat(&t.pat);
    v.visit_term(&t.term);
}
pub fn visit_term_let_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermLet) {
    v.visit_tpat_mut(&mut t.pat);
    v.visit_term_mut(&mut t.term);
}

pub fn visit_term_lit<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermLit) {
    v.visit_lit(&t.lit);
}
pub fn visit_term_lit_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermLit) {
    v.visit_lit_mut(&mut t.lit);
}

pub fn visit_term_log_eq<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermLogEq) {
    v.visit_term(&t.lhs);
    v.visit_term(&t.rhs);
}
pub fn visit_term_log_eq_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermLogEq) {
    v.visit_term_mut(&mut t.lhs);
    v.visit_term_mut(&mut t.rhs);
}

pub fn visit_term_match<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermMatch) {
    v.visit_term(&t.term);
    for a in &t.arms {
        v.visit_term_arm(a);
    }
}
pub fn visit_term_match_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermMatch) {
    v.visit_term_mut(&mut t.term);
    for a in &mut t.arms {
        v.visit_term_arm_mut(a);
    }
}

pub fn visit_term_method_call<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermMethodCall) {
    v.visit_term(&t.receiver);
    v.visit_ident(&t.method);
    if let Some(t) = &t.turbofish {
        v.visit_term_angle_bracketed_generic_arguments(t)
    }
    for a in &t.args {
        v.visit_term(a);
    }
}
pub fn visit_term_method_call_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermMethodCall) {
    v.visit_term_mut(&mut t.receiver);
    v.visit_ident_mut(&mut t.method);
    if let Some(t) = &mut t.turbofish {
        v.visit_term_angle_bracketed_generic_arguments_mut(t)
    }
    for a in &mut t.args {
        v.visit_term_mut(a);
    }
}

pub fn visit_term_model<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermModel) {
    v.visit_term(&t.term)
}
pub fn visit_term_model_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermModel) {
    v.visit_term_mut(&mut t.term)
}

pub fn visit_term_old<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermOld) {
    v.visit_term(&t.term)
}
pub fn visit_term_old_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermOld) {
    v.visit_term_mut(&mut t.term)
}

pub fn visit_term_paren<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermParen) {
    v.visit_term(&t.term)
}
pub fn visit_term_paren_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermParen) {
    v.visit_term_mut(&mut t.term)
}

pub fn visit_term_path<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermPath) {
    v.visit_expr_path(&t.inner)
}
pub fn visit_term_path_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermPath) {
    v.visit_expr_path_mut(&mut t.inner)
}

pub fn visit_term_range<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermRange) {
    if let Some(start) = &t.start {
        v.visit_term(start);
    }
    if let Some(end) = &t.end {
        v.visit_term(end)
    }
}
pub fn visit_term_range_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermRange) {
    if let Some(start) = &mut t.start {
        v.visit_term_mut(start);
    }
    if let Some(end) = &mut t.end {
        v.visit_term_mut(end)
    }
}

pub fn visit_term_repeat<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermRepeat) {
    v.visit_term(&t.term);
    v.visit_term(&t.len);
}
pub fn visit_term_repeat_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermRepeat) {
    v.visit_term_mut(&mut t.term);
    v.visit_term_mut(&mut t.len);
}

pub fn visit_term_struct<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermStruct) {
    v.visit_path(&t.path);
    for f in &t.fields {
        v.visit_term_field_value(f);
    }
    if let Some(rest) = &t.rest {
        v.visit_term(rest)
    }
}
pub fn visit_term_struct_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermStruct) {
    v.visit_path_mut(&mut t.path);
    for f in &mut t.fields {
        v.visit_term_field_value_mut(f);
    }
    if let Some(rest) = &mut t.rest {
        v.visit_term_mut(rest)
    }
}

pub fn visit_term_tuple<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermTuple) {
    for t in &t.elems {
        v.visit_term(t);
    }
}
pub fn visit_term_tuple_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermTuple) {
    for t in &mut t.elems {
        v.visit_term_mut(t);
    }
}

pub fn visit_term_unary<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermUnary) {
    v.visit_un_op(&t.op);
    v.visit_term(&t.term);
}
pub fn visit_term_unary_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermUnary) {
    v.visit_un_op_mut(&mut t.op);
    v.visit_term_mut(&mut t.term);
}

pub fn visit_tblock<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TBlock) {
    for s in &t.stmts {
        v.visit_term_stmt(s);
    }
}
pub fn visit_tblock_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TBlock) {
    for s in &mut t.stmts {
        v.visit_term_stmt_mut(s);
    }
}

pub fn visit_term_stmt<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermStmt) {
    match t {
        TermStmt::Local(s) => v.visit_tlocal(s),
        TermStmt::Item(s) => v.visit_item(s),
        TermStmt::Term(s) => v.visit_term(s),
        TermStmt::Semi(s, _) => v.visit_term(s),
    }
}
pub fn visit_term_stmt_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermStmt) {
    match t {
        TermStmt::Local(s) => v.visit_tlocal_mut(s),
        TermStmt::Item(s) => v.visit_item_mut(s),
        TermStmt::Term(s) => v.visit_term_mut(s),
        TermStmt::Semi(s, _) => v.visit_term_mut(s),
    }
}

pub fn visit_tlocal<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TLocal) {
    v.visit_tpat(&t.pat);
    if let Some((_, init)) = &t.init {
        v.visit_term(init)
    }
}
pub fn visit_tlocal_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TLocal) {
    v.visit_tpat_mut(&mut t.pat);
    if let Some((_, init)) = &mut t.init {
        v.visit_term_mut(init)
    }
}

pub fn visit_term_arm<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermArm) {
    v.visit_tpat(&t.pat);
    if let Some((_, guard)) = &t.guard {
        v.visit_term(guard);
    }
    v.visit_term(&t.body);
}
pub fn visit_term_arm_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermArm) {
    v.visit_tpat_mut(&mut t.pat);
    if let Some((_, guard)) = &mut t.guard {
        v.visit_term_mut(guard);
    }
    v.visit_term_mut(&mut t.body);
}

pub fn visit_term_field_value<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast TermFieldValue) {
    v.visit_member(&t.member);
    v.visit_term(&t.term);
}
pub fn visit_term_field_value_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermFieldValue) {
    v.visit_member_mut(&mut t.member);
    v.visit_term_mut(&mut t.term);
}

pub fn visit_term_angle_bracketed_generic_arguments<'ast, V: Visit<'ast> + ?Sized>(
    v: &mut V,
    t: &'ast TermAngleBracketedGenericArguments,
) {
    for a in &t.args {
        v.visit_term_generic_method_argument(a);
    }
}
pub fn visit_term_angle_bracketed_generic_arguments_mut<V: VisitMut + ?Sized>(
    v: &mut V,
    t: &mut TermAngleBracketedGenericArguments,
) {
    for a in &mut t.args {
        v.visit_term_generic_method_argument_mut(a);
    }
}

pub fn visit_term_generic_method_argument<'ast, V: Visit<'ast> + ?Sized>(
    v: &mut V,
    t: &'ast TermGenericMethodArgument,
) {
    match t {
        TermGenericMethodArgument::Type(t) => v.visit_type(t),
        TermGenericMethodArgument::Const(t) => v.visit_term(t),
    }
}
pub fn visit_term_generic_method_argument_mut<V: VisitMut + ?Sized>(
    v: &mut V,
    t: &mut TermGenericMethodArgument,
) {
    match t {
        TermGenericMethodArgument::Type(t) => v.visit_type_mut(t),
        TermGenericMethodArgument::Const(t) => v.visit_term_mut(t),
    }
}

pub fn visit_quant_arg<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast QuantArg) {
    v.visit_ident(&t.ident);
    v.visit_type(&t.ty);
}
pub fn visit_quant_arg_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut QuantArg) {
    v.visit_ident_mut(&mut t.ident);
    v.visit_type_mut(&mut t.ty);
}

pub fn visit_tpat<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast Pat) {
    match t {
        Pat::Ident(p) => v.visit_tpat_ident(p),
        Pat::Lit(p) => v.visit_term_lit(p),
        Pat::Macro(p) => v.visit_expr_macro(p),
        Pat::Or(p) => v.visit_tpat_or(p),
        Pat::Paren(p) => v.visit_tpat_paren(p),
        Pat::Path(p) => v.visit_term_path(p),
        Pat::Range(p) => v.visit_term_range(p),
        Pat::Rest(p) => v.visit_tpat_rest(p),
        Pat::Slice(p) => v.visit_tpat_slice(p),
        Pat::Struct(p) => v.visit_tpat_struct(p),
        Pat::Tuple(p) => v.visit_tpat_tuple(p),
        Pat::TupleStruct(p) => v.visit_tpat_tuple_struct(p),
        Pat::Type(p) => v.visit_tpat_type(p),
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(p) => v.visit_tpat_wild(p),
    }
}
pub fn visit_tpat_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut Pat) {
    match t {
        Pat::Ident(p) => v.visit_tpat_ident_mut(p),
        Pat::Lit(p) => v.visit_term_lit_mut(p),
        Pat::Macro(p) => v.visit_expr_macro_mut(p),
        Pat::Or(p) => v.visit_tpat_or_mut(p),
        Pat::Paren(p) => v.visit_tpat_paren_mut(p),
        Pat::Path(p) => v.visit_term_path_mut(p),
        Pat::Range(p) => v.visit_term_range_mut(p),
        Pat::Rest(p) => v.visit_tpat_rest_mut(p),
        Pat::Slice(p) => v.visit_tpat_slice_mut(p),
        Pat::Struct(p) => v.visit_tpat_struct_mut(p),
        Pat::Tuple(p) => v.visit_tpat_tuple_mut(p),
        Pat::TupleStruct(p) => v.visit_tpat_tuple_struct_mut(p),
        Pat::Type(p) => v.visit_tpat_type_mut(p),
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(p) => v.visit_tpat_wild_mut(p),
    }
}

pub fn visit_tpat_ident<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast PatIdent) {
    v.visit_ident(&t.ident);
    if let Some((_, p)) = &t.subpat {
        v.visit_tpat(p);
    }
}
pub fn visit_tpat_ident_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatIdent) {
    v.visit_ident_mut(&mut t.ident);
    if let Some((_, p)) = &mut t.subpat {
        v.visit_tpat_mut(p);
    }
}

pub fn visit_tpat_or<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast PatOr) {
    for c in &t.cases {
        v.visit_tpat(c);
    }
}
pub fn visit_tpat_or_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatOr) {
    for c in &mut t.cases {
        v.visit_tpat_mut(c);
    }
}

pub fn visit_tpat_paren<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast PatParen) {
    v.visit_tpat(&t.pat)
}
pub fn visit_tpat_paren_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatParen) {
    v.visit_tpat_mut(&mut t.pat)
}

pub fn visit_tpat_rest<'ast, V: Visit<'ast> + ?Sized>(_: &mut V, _: &'ast PatRest) {}
pub fn visit_tpat_rest_mut<V: VisitMut + ?Sized>(_: &mut V, _: &mut PatRest) {}

pub fn visit_tpat_slice<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast PatSlice) {
    for c in &t.elems {
        v.visit_tpat(c);
    }
}
pub fn visit_tpat_slice_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatSlice) {
    for c in &mut t.elems {
        v.visit_tpat_mut(c);
    }
}

pub fn visit_tpat_struct<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast PatStruct) {
    if let Some(q) = &t.qself {
        v.visit_qself(q);
    }
    v.visit_path(&t.path);
    for f in &t.fields {
        v.visit_tfield_pat(f);
    }
    if let Some(r) = &t.rest {
        v.visit_tpat_rest(r);
    }
}
pub fn visit_tpat_struct_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatStruct) {
    if let Some(q) = &mut t.qself {
        v.visit_qself_mut(q);
    }
    v.visit_path_mut(&mut t.path);
    for f in &mut t.fields {
        v.visit_tfield_pat_mut(f);
    }
    if let Some(r) = &mut t.rest {
        v.visit_tpat_rest_mut(r);
    }
}

pub fn visit_tpat_tuple<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast PatTuple) {
    for e in &t.elems {
        v.visit_tpat(e);
    }
}
pub fn visit_tpat_tuple_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatTuple) {
    for e in &mut t.elems {
        v.visit_tpat_mut(e);
    }
}

pub fn visit_tpat_tuple_struct<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast PatTupleStruct) {
    if let Some(q) = &t.qself {
        v.visit_qself(q);
    }
    v.visit_path(&t.path);
    for e in &t.elems {
        v.visit_tpat(e);
    }
}
pub fn visit_tpat_tuple_struct_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatTupleStruct) {
    if let Some(q) = &mut t.qself {
        v.visit_qself_mut(q);
    }
    v.visit_path_mut(&mut t.path);
    for e in &mut t.elems {
        v.visit_tpat_mut(e);
    }
}

pub fn visit_tpat_type<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast PatType) {
    v.visit_tpat(&t.pat);
    v.visit_type(&t.ty);
}
pub fn visit_tpat_type_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatType) {
    v.visit_tpat_mut(&mut t.pat);
    v.visit_type_mut(&mut t.ty);
}

pub fn visit_tpat_wild<'ast, V: Visit<'ast> + ?Sized>(_: &mut V, _: &'ast PatWild) {}
pub fn visit_tpat_wild_mut<V: VisitMut + ?Sized>(_: &mut V, _: &mut PatWild) {}

pub fn visit_tfield_pat<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast FieldPat) {
    v.visit_member(&t.member);
    v.visit_tpat(&t.pat);
}
pub fn visit_tfield_pat_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut FieldPat) {
    v.visit_member_mut(&mut t.member);
    v.visit_tpat_mut(&mut t.pat);
}

pub fn visit_loc_set<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast LocSet) {
    match t {
        LocSet::Field(l) => v.visit_loc_set_field(l),
        LocSet::FieldWildcard(l) => v.visit_loc_set_field_wildcard(l),
        LocSet::Index(l) => v.visit_loc_set_index(l),
        LocSet::Path(l) => v.visit_loc_set_path(l),
        LocSet::Group(l) => v.visit_loc_set_group(l),
        LocSet::Nothing(l) => v.visit_loc_set_nothing(l),
    }
}
pub fn visit_loc_set_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut LocSet) {
    match t {
        LocSet::Field(l) => v.visit_loc_set_field_mut(l),
        LocSet::FieldWildcard(l) => v.visit_loc_set_field_wildcard_mut(l),
        LocSet::Index(l) => v.visit_loc_set_index_mut(l),
        LocSet::Path(l) => v.visit_loc_set_path_mut(l),
        LocSet::Group(l) => v.visit_loc_set_group_mut(l),
        LocSet::Nothing(l) => v.visit_loc_set_nothing_mut(l),
    }
}

pub fn visit_loc_set_field<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast LocSetField) {
    v.visit_term(&t.base);
    v.visit_member(&t.member);
}
pub fn visit_loc_set_field_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut LocSetField) {
    v.visit_term_mut(&mut t.base);
    v.visit_member_mut(&mut t.member);
}

pub fn visit_loc_set_field_wildcard<'ast, V: Visit<'ast> + ?Sized>(
    v: &mut V,
    t: &'ast LocSetFieldWildcard,
) {
    v.visit_term(&t.base);
}
pub fn visit_loc_set_field_wildcard_mut<V: VisitMut + ?Sized>(
    v: &mut V,
    t: &mut LocSetFieldWildcard,
) {
    v.visit_term_mut(&mut t.base);
}

pub fn visit_loc_set_index<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast LocSetIndex) {
    v.visit_term(&t.term);
    v.visit_term(&t.index);
}
pub fn visit_loc_set_index_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut LocSetIndex) {
    v.visit_term_mut(&mut t.term);
    v.visit_term_mut(&mut t.index);
}

pub fn visit_loc_set_path<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast LocSetPath) {
    v.visit_term_path(&t.inner);
}
pub fn visit_loc_set_path_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut LocSetPath) {
    v.visit_term_path_mut(&mut t.inner);
}

pub fn visit_loc_set_group<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast LocSetGroup) {
    for l in &t.items {
        v.visit_loc_set(l);
    }
}
pub fn visit_loc_set_group_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut LocSetGroup) {
    for l in &mut t.items {
        v.visit_loc_set_mut(l);
    }
}

pub fn visit_loc_set_nothing<'ast, V: Visit<'ast> + ?Sized>(_: &mut V, _: &'ast LocSetNothing) {}
pub fn visit_loc_set_nothing_mut<V: VisitMut + ?Sized>(_: &mut V, _: &mut LocSetNothing) {}

pub fn visit_spec<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, t: &'ast Spec) {
    for p in &t.pre_conds {
        v.visit_term(p);
    }
    if let Some(l) = &t.modifies {
        v.visit_loc_set(l);
    }
    if let Some(var) = &t.variant {
        v.visit_term(var);
    }
    if let Some(Some(d)) = &t.diverges {
        v.visit_term(d);
    }
    for p in &t.post_conds {
        v.visit_term(p);
    }
    for (p1, p2) in &t.dem_conds {
        v.visit_term(p1);
        v.visit_term(p2);
    }
}
pub fn visit_spec_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut Spec) {
    for p in &mut t.pre_conds {
        v.visit_term_mut(p);
    }
    if let Some(l) = &mut t.modifies {
        v.visit_loc_set_mut(l);
    }
    if let Some(var) = &mut t.variant {
        v.visit_term_mut(var);
    }
    if let Some(Some(d)) = &mut t.diverges {
        v.visit_term_mut(d);
    }
    for p in &mut t.post_conds {
        v.visit_term_mut(p);
    }
    for (p1, p2) in &mut t.dem_conds {
        v.visit_term_mut(p1);
        v.visit_term_mut(p2);
    }
}
