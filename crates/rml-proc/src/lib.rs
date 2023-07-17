use proc_macro::TokenStream as TS1;
use proc_macro2::{Span, TokenStream as TS2};
use quote::{quote, quote_spanned};
use rml_syn::{Encode, Spec, Term};

use syn::{
    parse_macro_input, parse_quote, parse_quote_spanned, Attribute, FnArg, Ident, ItemFn, Pat,
    ReturnType, Signature, Type,
};

mod subject;

use subject::{ContractSubject, InvariantSubject};

#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    let sp = parse_macro_input!(attr as Spec);
    //println!("{:#?}", sp);

    let item = parse_macro_input!(item as ContractSubject);

    let spec_name = generate_unique_ident(&item.name());
    let name_tag = format!("{}", quote! { #spec_name });

    match item {
        ContractSubject::FnOrMethod(fn_or_meth) if fn_or_meth.is_trait_signature() => {
            TS1::from(quote! {
                #fn_or_meth
            })
        }
        ContractSubject::FnOrMethod(fn_or_meth) => {
            let result = match fn_or_meth.sig.output {
                ReturnType::Default => parse_quote! { result : () },
                ReturnType::Type(_, ref ty) => parse_quote! { result : #ty },
            };
            let spec_tokens = fn_spec_item(
                spec_name,
                fn_or_meth.sig.clone(),
                result,
                sp,
                Span::call_site(),
            );
            TS1::from(quote! {
                #spec_tokens
                #[rml::specification::spec=#name_tag]
                #fn_or_meth
            })
        }
        ContractSubject::Closure(c) => TS1::from(quote! {
            #c
        }),
    }
}

#[proc_macro_attribute]
pub fn pure(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty());
    let toks = TS2::from(item);
    TS1::from(quote! {
        #[rml::decl::pure]
        #toks
    })
}

#[proc_macro_attribute]
pub fn invariant(attr: TS1, item: TS1) -> TS1 {
    let term = parse_macro_input!(attr as Term);
    let subject = parse_macro_input!(item as InvariantSubject);
    let ts = match subject {
        InvariantSubject::ForLoop(l) => {
            quote! {#l}
        }
        InvariantSubject::Loop(l) => {
            let inv = term.encode();
            quote! {
                #inv
                #l
            }
        }
        InvariantSubject::While(l) => quote! {
            #l
        },
        InvariantSubject::Trait(i) => quote! {#i},
        InvariantSubject::Struct(i) => quote! {#i},
        InvariantSubject::Enum(i) => quote! {#i},
    };

    TS1::from(ts)
}

#[proc_macro_attribute]
pub fn variant(_attr: TS1, item: TS1) -> TS1 {
    item
}

#[proc_macro_attribute]
pub fn modifies(_attr: TS1, item: TS1) -> TS1 {
    item
}

fn generate_unique_ident(prefix: &str) -> Ident {
    let uuid = uuid::Uuid::new_v4();
    let ident = format!("{}_{}", prefix, uuid).replace('-', "_");

    Ident::new(&ident, Span::call_site())
}

fn fn_spec_item(
    spec_name: Ident,
    sig: Signature,
    result: FnArg,
    mut spec: Spec,
    span: Span,
) -> TS2 {
    let is_normal = spec.is_normal();
    let spec_attr = if is_normal {
        "spec_normal"
    } else {
        "spec_panic"
    };
    let spec_attr = Ident::new(spec_attr, Span::call_site());
    let mut post_sig = sig.clone();
    post_sig.inputs.push(result);

    // pre
    if spec.pre_conds.is_empty() {
        spec.pre_conds.push(parse_quote!(true));
    }
    let pre_idents: Vec<_> = (0..spec.pre_conds.len())
        .map(|_| generate_unique_ident("spec_part_pre"))
        .collect();
    let pre: Vec<_> = spec
        .pre_conds
        .into_iter()
        .enumerate()
        .map(|(i, p)| {
            let id = &pre_idents[i];
            let t = p.encode();
            let mut res: ItemFn = parse_quote_spanned! { span =>
                #[allow(unused_variables)]
                fn #id() -> bool {
                    let cond: bool = !!(#t);
                    cond
                }
            };
            adapt_sig(&mut res.sig, &sig);

            res
        })
        .collect();
    let pre_strs = pre_idents.iter().map(|i| i.to_string());

    //post
    if spec.post_conds.is_empty() {
        spec.post_conds.push(parse_quote!(true))
    }
    let post_idents: Vec<_> = (0..spec.post_conds.len())
        .map(|_| generate_unique_ident("spec_part_post"))
        .collect();
    let post = spec.post_conds.into_iter().enumerate().map(|(i, p)| {
        let id = &post_idents[i];
        let t = p.encode();
        let mut res: ItemFn = parse_quote_spanned! { span =>
            #[allow(unused_variables)]
            fn #id() -> bool {
                let cond: bool = !!(#t);
                cond
            }
        };
        adapt_sig(&mut res.sig, &post_sig);

        res
    });
    let post_strs = post_idents.iter().map(|i| i.to_string());

    // TODO: modifies

    // variant
    let (var_attr, var) = if let Some(v) = spec.variant {
        let t = v.encode();
        let id = generate_unique_ident("spec_part_var");
        let mut item: ItemFn = parse_quote_spanned! { span =>
            #[allow(unused_variables)]
            fn #id() -> impl ::rml_contracts::WellFounded {
                #t
            }
        };
        adapt_sig(&mut item.sig, &sig);
        let id_str = id.to_string();
        let var_attr: Attribute = parse_quote_spanned! { span => #[rml::spec_part_var=#id_str] };
        (Some(var_attr), Some(item))
    } else {
        (None, None)
    };

    // diverges
    let diverges = spec
        .diverges
        .map(|d| d.unwrap_or_else(|| parse_quote_spanned! { span => true }))
        .unwrap_or_else(|| parse_quote_spanned! { span => false });
    let (div, div_attr) = {
        let id = generate_unique_ident("spec_part_div");
        let t = diverges.encode();

        let mut item: ItemFn = parse_quote_spanned! { span =>
            #[allow(unused_variables)]
            fn #id() -> bool {
                let b: bool = #t;
                b
            }
        };
        adapt_sig(&mut item.sig, &sig);
        let id_str = id.to_string();
        let attr: Attribute = parse_quote_spanned! { span => #[rml::spec_part_div=#id_str] };
        (item, attr)
    };

    let spec_name_str = spec_name.to_string();
    quote_spanned! { span =>
        #(#pre)*
        #(#post)*
        #div
        #var
        #[allow(unused_must_use, unused_variables, dead_code)]
        #[rml::#spec_attr=#spec_name_str]
        #(#[rml::spec_part_pre_ref=#pre_strs])*
        #(#[rml::spec_part_post_ref=#post_strs])*
        #var_attr
        #div_attr
        const #spec_name: bool = false;
    }
}

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
