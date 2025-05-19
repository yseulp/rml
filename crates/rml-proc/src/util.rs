use proc_macro2::Span;
use rml_syn::{
    visit::{MutVisitable, VisitMut},
    Encode, LocSet, Term,
};
use syn::{
    parse_quote, parse_quote_spanned,
    punctuated::{Pair, Punctuated},
    Attribute, FnArg, Generics, Ident, ItemFn, Pat, PatType, Path, Receiver, Signature, Stmt,
    Token, Type, TypeReference,
};

/// Default, empty generics.
///
/// (Why does [Generics] not implement [Default]?)
pub(crate) const EMPTY_GENERICS: Generics = Generics {
    lt_token: None,
    params: Punctuated::new(),
    gt_token: None,
    where_clause: None,
};

pub(crate) fn get_mut_ref_params(sig: &Signature) -> impl Iterator<Item = &FnArg> {
    sig.inputs.iter().filter(|a| {
        matches!(
            a,
            FnArg::Receiver(Receiver {
                reference: Some(_),
                mutability: Some(_),
                ..
            }) | FnArg::Typed(PatType {
                ty: box Type::Reference(TypeReference {
                    mutability: Some(_),
                    ..
                }),
                ..
            })
        )
    })
}

/// Generates a random [Ident] with the `prefix`.
pub(crate) fn gen_unique_ident(prefix: &str) -> Ident {
    let uuid = uuid::Uuid::new_v4();
    let ident = format!("{prefix}_{uuid}").replace('-', "_");

    Ident::new(&ident, Span::call_site())
}

/// Extract all attributes with identifier `attr`.
pub(crate) fn extract_attrs(attrs: &mut Vec<Attribute>, attr: &str) -> Vec<Attribute> {
    attrs
        .extract_if(.., |a| {
            a.path().get_ident().map(|i| i == attr).unwrap_or(false)
        })
        .collect()
}

/// Generate a generic function with `ident`, return type `ret_ty`, `body`. The
/// result will have span `span`.
///
/// The signature get adapted to `inputs` and `generics`. See [adapt_sig].
pub(crate) fn gen_spec_fn<'a>(
    ident: &Ident,
    span: Span,
    attr_path: &Path,
    ret_ty: &Type,
    body: &[Stmt],
    inputs: impl Iterator<Item = Pair<&'a FnArg, &'a Token![,]>>,
    generics: &'a Generics,
) -> ItemFn {
    let ident_str = ident.to_string();
    let mut f: ItemFn = parse_quote_spanned! { span =>
        #[allow(unused_variables, dead_code)]
        #[#attr_path=#ident_str]
        fn #ident() -> #ret_ty {
            #(#body)*
        }
    };

    adapt_sig(&mut f.sig, inputs, generics);

    f
}

/// Generate a spec function that returns booean `term` (encoded). See
/// [gen_spec_fn].
pub(crate) fn gen_bool_spec_fn<'a>(
    ident: &Ident,
    span: Span,
    term: Term,
    attr_path: &Path,
    inputs: impl Iterator<Item = Pair<&'a FnArg, &'a Token![,]>>,
    generics: &'a Generics,
) -> ItemFn {
    let e = term.encode();
    gen_spec_fn(
        ident,
        span,
        attr_path,
        &parse_quote!(bool),
        &vec![
            parse_quote!(let cond: bool = !!(#e);),
            Stmt::Expr(parse_quote!(cond), None),
        ],
        inputs,
        generics,
    )
}

/// Generate a spec function that returns locset `ls` (encoded). See
/// [gen_spec_fn].
pub(crate) fn gen_locset_spec_fn<'a>(
    ident: &Ident,
    span: Span,
    ls: LocSet,
    attr_path: &Path,
    inputs: impl Iterator<Item = Pair<&'a FnArg, &'a Token![,]>>,
    generics: &'a Generics,
) -> ItemFn {
    let e = ls.encode();
    gen_spec_fn(
        ident,
        span,
        attr_path,
        &parse_quote!(::rml_contracts::logic::LocSet),
        &vec![Stmt::Expr(parse_quote!(#e), None)],
        inputs,
        generics,
    )
}

/// Generate a spec function that returns a well founded `term` (encoded). See
/// [gen_spec_fn].
pub(crate) fn gen_wf_spec_fn<'a>(
    ident: &Ident,
    span: Span,
    term: Term,
    attr_path: &Path,
    inputs: impl Iterator<Item = Pair<&'a FnArg, &'a Token![,]>>,
    generics: &'a Generics,
) -> ItemFn {
    let e = term.encode();
    gen_spec_fn(
        ident,
        span,
        attr_path,
        &parse_quote!(impl ::rml_contracts::WellFounded),
        &vec![Stmt::Expr(parse_quote!(#e), None)],
        inputs,
        generics,
    )
}

/// Adapt the signature `sig` to `old_sig`, i.e.:
/// - Copy `old_sig`'s parameters but remove mutability.
/// - Copy generics.
fn adapt_sig<'a>(
    sig: &mut Signature,
    inputs: impl Iterator<Item = Pair<&'a FnArg, &'a Token![,]>>,
    generics: &'a Generics,
) {
    for p in inputs {
        let (arg, punct) = p.into_tuple();
        match arg.clone() {
            FnArg::Receiver(mut r) => {
                r.mutability = None;
                sig.inputs.push_value(FnArg::Receiver(r));
            }
            FnArg::Typed(mut a) => {
                a.pat = match *a.pat {
                    Pat::Ident(mut p) => {
                        p.mutability = None;
                        Pat::Ident(p).into()
                    }
                    p => p.into(),
                };
                a.ty = match *a.ty {
                    Type::Reference(mut r) => {
                        r.mutability = None;
                        Type::Reference(r).into()
                    }
                    ty => ty.into(),
                };
                sig.inputs.push_value(FnArg::Typed(a));
            }
        }
        if let Some(punct) = punct {
            sig.inputs.push_punct(*punct);
        }
    }

    sig.generics = generics.clone();
}

/// Generate a punctuated parameter list just containing `self`.
pub(crate) fn gen_self_params() -> Punctuated<syn::FnArg, Token![,]> {
    let mut inputs = Punctuated::new();
    inputs.push(parse_quote!(self));
    inputs
}

struct SelfReplacer<'a>(&'a Ident);

impl<'a> syn::visit_mut::VisitMut for SelfReplacer<'a> {
    fn visit_ident_mut(&mut self, i: &mut Ident) {
        if *i == Ident::new("self", i.span()) {
            *i = self.0.clone();
        }
    }
}

impl<'a> VisitMut for SelfReplacer<'a> {}

pub(crate) fn replace_self<T>(target: &mut T, replace_with: &Ident)
where
    T: MutVisitable,
{
    let mut r = SelfReplacer(replace_with);
    target.visit(&mut r);
}
