use paste::paste;

use crate::{
    FieldPat, Pat, PatIdent, PatOr, PatParen, PatRest, PatSlice, PatStruct, PatTuple,
    PatTupleStruct, PatType, PatWild, QuantArg, TBlock, TLocal, Term,
    TermAngleBracketedGenericArguments, TermArm, TermArray, TermBinary, TermBlock, TermCall,
    TermCast, TermClosure, TermExists, TermField, TermFieldValue, TermForall,
    TermGenericMethodArgument, TermGroup, TermIf, TermImpl, TermIndex, TermLet, TermLit, TermLogEq,
    TermMatch, TermMethodCall, TermModel, TermOld, TermParen, TermPath, TermRange, TermRepeat,
    TermStmt, TermStruct, TermTuple, TermUnary,
};

macro_rules! visit_mut {
    ($($name:ident : $ty:ty), *) => {
        $(
            paste!{
                fn [< visit_ $name _mut >] (&mut self, t: &mut $ty) {
                    [< visit_ $name _mut >](self, t)
                }
            }
        )*
    };
}

pub trait VisitMut: syn::visit_mut::VisitMut {
    visit_mut! {
        term: Term,
        term_array: TermArray,
        term_binary: TermBinary,
        term_block: TermBlock,
        term_call: TermCall,
        term_cast: TermCast,
        term_closure: TermClosure,
        term_exists: TermExists,
        term_field: TermField,
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

        tfield_pat: FieldPat
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

pub fn visit_term_array_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermArray) {
    for e in &mut t.elems {
        v.visit_term_mut(e)
    }
}

pub fn visit_term_binary_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermBinary) {
    v.visit_term_mut(&mut t.left);
    v.visit_bin_op_mut(&mut t.op);
    v.visit_term_mut(&mut t.right);
}

pub fn visit_term_block_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermBlock) {
    if let Some(l) = &mut t.label {
        v.visit_label_mut(l);
    }
    v.visit_tblock_mut(&mut t.block);
}

pub fn visit_term_call_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermCall) {
    v.visit_term_mut(&mut t.func);
    for a in &mut t.args {
        v.visit_term_mut(a);
    }
}
pub fn visit_term_cast_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermCast) {
    v.visit_term_mut(&mut t.term);
    v.visit_type_mut(&mut t.ty);
}
pub fn visit_term_closure_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermClosure) {
    for p in &mut t.inputs {
        v.visit_tpat_mut(p);
    }
    v.visit_return_type_mut(&mut t.output);
    v.visit_term_mut(&mut t.body);
}
pub fn visit_term_exists_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermExists) {
    for a in &mut t.args {
        v.visit_quant_arg_mut(a);
    }
    v.visit_term_mut(&mut t.term);
}
pub fn visit_term_field_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermField) {
    v.visit_term_mut(&mut t.base);
    v.visit_member_mut(&mut t.member);
}
pub fn visit_term_forall_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermForall) {
    for a in &mut t.args {
        v.visit_quant_arg_mut(a);
    }
    v.visit_term_mut(&mut t.term);
}
pub fn visit_term_group_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermGroup) {
    v.visit_term_mut(&mut t.term);
}
pub fn visit_term_if_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermIf) {
    v.visit_term_mut(&mut t.cond);
    v.visit_tblock_mut(&mut t.then_branch);
    if let Some(e) = &mut t.else_branch {
        v.visit_term_mut(&mut e.1);
    }
}
pub fn visit_term_impl_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermImpl) {
    v.visit_term_mut(&mut t.hyp);
    v.visit_term_mut(&mut t.cons);
}
pub fn visit_term_index_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermIndex) {
    v.visit_term_mut(&mut t.term);
    v.visit_term_mut(&mut t.index);
}
pub fn visit_term_let_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermLet) {
    v.visit_tpat_mut(&mut t.pat);
    v.visit_term_mut(&mut t.term);
}
pub fn visit_term_lit_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermLit) {
    v.visit_lit_mut(&mut t.lit);
}
pub fn visit_term_log_eq_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermLogEq) {
    v.visit_term_mut(&mut t.lhs);
    v.visit_term_mut(&mut t.rhs);
}
pub fn visit_term_match_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermMatch) {
    v.visit_term_mut(&mut t.term);
    for a in &mut t.arms {
        v.visit_term_arm_mut(a);
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
pub fn visit_term_model_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermModel) {
    v.visit_term_mut(&mut t.term)
}
pub fn visit_term_old_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermOld) {
    v.visit_term_mut(&mut t.term)
}
pub fn visit_term_paren_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermParen) {
    v.visit_term_mut(&mut t.term)
}
pub fn visit_term_path_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermPath) {
    v.visit_expr_path_mut(&mut t.inner)
}
pub fn visit_term_range_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermRange) {
    if let Some(start) = &mut t.start {
        v.visit_term_mut(start);
    }
    if let Some(end) = &mut t.end {
        v.visit_term_mut(end)
    }
}
pub fn visit_term_repeat_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermRepeat) {
    v.visit_term_mut(&mut t.term);
    v.visit_term_mut(&mut t.len);
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
pub fn visit_term_tuple_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermTuple) {
    for t in &mut t.elems {
        v.visit_term_mut(t);
    }
}
pub fn visit_term_unary_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermUnary) {
    v.visit_un_op_mut(&mut t.op);
    v.visit_term_mut(&mut t.term);
}
pub fn visit_tblock_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TBlock) {
    for s in &mut t.stmts {
        v.visit_term_stmt_mut(s);
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
pub fn visit_tlocal_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TLocal) {
    v.visit_tpat_mut(&mut t.pat);
    if let Some((_, init)) = &mut t.init {
        v.visit_term_mut(init)
    }
}
pub fn visit_term_arm_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermArm) {
    v.visit_tpat_mut(&mut t.pat);
    if let Some((_, guard)) = &mut t.guard {
        v.visit_term_mut(guard);
    }
    v.visit_term_mut(&mut t.body);
}
pub fn visit_term_field_value_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut TermFieldValue) {
    v.visit_member_mut(&mut t.member);
    v.visit_term_mut(&mut t.term);
}
pub fn visit_term_angle_bracketed_generic_arguments_mut<V: VisitMut + ?Sized>(
    v: &mut V,
    t: &mut TermAngleBracketedGenericArguments,
) {
    for a in &mut t.args {
        v.visit_term_generic_method_argument_mut(a);
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
pub fn visit_quant_arg_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut QuantArg) {
    v.visit_ident_mut(&mut t.ident);
    v.visit_type_mut(&mut t.ty);
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
pub fn visit_tpat_ident_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatIdent) {
    v.visit_ident_mut(&mut t.ident);
    if let Some((_, p)) = &mut t.subpat {
        v.visit_tpat_mut(p);
    }
}
pub fn visit_tpat_or_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatOr) {
    for c in &mut t.cases {
        v.visit_tpat_mut(c);
    }
}
pub fn visit_tpat_paren_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatParen) {
    v.visit_tpat_mut(&mut t.pat)
}
pub fn visit_tpat_rest_mut<V: VisitMut + ?Sized>(_: &mut V, _: &mut PatRest) {}
pub fn visit_tpat_slice_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatSlice) {
    for c in &mut t.elems {
        v.visit_tpat_mut(c);
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
pub fn visit_tpat_tuple_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatTuple) {
    for e in &mut t.elems {
        v.visit_tpat_mut(e);
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
pub fn visit_tpat_type_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut PatType) {
    v.visit_tpat_mut(&mut t.pat);
    v.visit_type_mut(&mut t.ty);
}
pub fn visit_tpat_wild_mut<V: VisitMut + ?Sized>(_: &mut V, _: &mut PatWild) {}
pub fn visit_tfield_pat_mut<V: VisitMut + ?Sized>(v: &mut V, t: &mut FieldPat) {
    v.visit_member_mut(&mut t.member);
    v.visit_tpat_mut(&mut t.pat);
}
