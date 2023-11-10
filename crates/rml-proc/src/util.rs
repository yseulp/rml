use proc_macro2::Span;
use rml_syn::{Encode, LocSet, Term};
use syn::{
    parse_quote, parse_quote_spanned, Attribute, FnArg, Ident, ItemFn, Pat, PatType, Path,
    Receiver, Signature, Stmt, Type, TypeReference,
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
pub(crate) fn generate_unique_ident(prefix: &str) -> Ident {
    let uuid = uuid::Uuid::new_v4();
    let ident = format!("{}_{}", prefix, uuid).replace('-', "_");

    Ident::new(&ident, Span::call_site())
}

/// Extract all attributes with identifier `attr`.
pub(crate) fn extract_attrs(attrs: &mut Vec<Attribute>, attr: &str) -> Vec<Attribute> {
    attrs
        .extract_if(|a| a.path().get_ident().map(|i| i == attr).unwrap_or(false))
        .collect()
}

/// Generate a generic function with `ident`, return type `ret_ty`, `body`. The
/// result will have span `span`.
///
/// The signature get adapted to `sig`. See [adapt_sig].
pub(crate) fn gen_spec_fn(
    ident: Ident,
    span: Span,
    attr_path: Path,
    ret_ty: Type,
    body: Vec<Stmt>,
    sig: &Signature,
) -> ItemFn {
    let ident_str = ident.to_string();
    let mut f: ItemFn = parse_quote_spanned! { span =>
        #[allow(unused_variables, dead_code)]
        #[#attr_path=#ident_str]
        fn #ident() -> #ret_ty {
            #(#body)*
        }
    };

    adapt_sig(&mut f.sig, sig);

    f
}

/// Generate a spec function that returns booean `term` (encoded). See
/// [gen_spec_fn].
pub(crate) fn gen_bool_spec_fn(
    ident: Ident,
    span: Span,
    term: Term,
    attr_path: Path,
    sig: &Signature,
) -> ItemFn {
    let e = term.encode();
    gen_spec_fn(
        ident,
        span,
        attr_path,
        parse_quote!(bool),
        vec![parse_quote!(let cond: bool = !!(#e);), parse_quote!(cond)],
        sig,
    )
}

/// Generate a spec function that returns locset `ls` (encoded). See
/// [gen_spec_fn].
pub(crate) fn gen_locset_spec_fn(
    ident: Ident,
    span: Span,
    ls: LocSet,
    attr_path: Path,
    sig: &Signature,
) -> ItemFn {
    let e = ls.encode();
    gen_spec_fn(
        ident,
        span,
        attr_path,
        parse_quote!(::rml_contracts::logic::LocSet),
        vec![parse_quote!(#e)],
        sig,
    )
}

/// Generate a spec function that returns a well founded `term` (encoded). See
/// [gen_spec_fn].
pub(crate) fn gen_wf_spec_fn(
    ident: Ident,
    span: Span,
    term: Term,
    attr_path: Path,
    sig: &Signature,
) -> ItemFn {
    let e = term.encode();
    gen_spec_fn(
        ident,
        span,
        attr_path,
        parse_quote!(impl ::rml_contracts::WellFounded),
        vec![parse_quote!(#e)],
        sig,
    )
}

/// Adapt the signature `sig` to `old_sig`, i.e.:
/// - Copy `old_sig`'s parameters but remove mutability.
/// - Copy generics.
fn adapt_sig(sig: &mut Signature, old_sig: &Signature) {
    for p in old_sig.inputs.pairs() {
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

    sig.generics = old_sig.generics.clone();
}
