#![feature(box_patterns)]
#![feature(extract_if)]

use proc_macro::TokenStream as TS1;
use proc_macro2::{Span, TokenStream as TS2};
use quote::{quote, quote_spanned, ToTokens};
use rml_syn::{
    locset::{LocSet, LocSetGroup},
    subject::LogicSubject,
    Encode, Spec, TBlock, Term,
};

use syn::{
    parse_macro_input, parse_quote, parse_quote_spanned, punctuated::Punctuated, spanned::Spanned,
    Attribute, Expr, FnArg, Ident, ItemFn, Meta, Pat, ReturnType, Signature, Stmt, Token, Type,
};

mod subject;
mod util;

use subject::{ContractSubject, InvariantSubject, LoopKind};

use crate::util::get_mut_ref_params;

#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    let sp = parse_macro_input!(attr as Spec);

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
                #[rml::spec_case_ref=#name_tag]
                #fn_or_meth
            })
        }
        ContractSubject::Closure(c) => TS1::from(quote! {
            #c
        }),
    }
}

#[proc_macro_attribute]
pub fn strictly_pure(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty(), "`strictly_pure` takes no arguments");
    let toks = TS2::from(item);
    TS1::from(quote! {
        #[rml::decl::strictly_pure]
        #toks
    })
}

#[proc_macro_attribute]
pub fn pure(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty(), "`pure` takes no arguments");
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
    let sp = subject.span();
    let ts = match subject {
        InvariantSubject::Loop(l) => {
            let (attrs, stmts, l) = loop_inv(term, l);
            quote_spanned! { sp=>
                {
                    #stmts
                    #attrs
                    #l
                }
            }
        }
        InvariantSubject::Item(_) => todo!(),
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

#[proc_macro_attribute]
pub fn logic(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty(), "`logic` takes no arguments");
    let subject = parse_macro_input!(item as LogicSubject);
    match subject {
        LogicSubject::WithBody(f) => {
            let sp = f.span();
            TS1::from(quote_spanned! { sp =>
                #[rml::decl::logic]
                #f
            })
        }
        LogicSubject::WithoutBody(t) => {
            let sp = t.span();
            TS1::from(quote_spanned! { sp =>
                #[rml::decl::logic]
                #t
            })
        }
    }
}

#[proc_macro_attribute]
pub fn trusted(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty(), "`trusted` takes no arguments");
    let toks = TS2::from(item);
    TS1::from(quote! {
        #[rml::delc::trusted]
        #toks
    })
}

#[proc_macro]
pub fn rml(tokens: TS1) -> TS1 {
    let block = parse_macro_input!(tokens with TBlock::parse_within);
    TS1::from(
        block
            .into_iter()
            .map(|ts| {
                let sp = ts.span();
                let stmt = ts.encode();
                quote_spanned! { sp => #stmt }
            })
            .collect::<TS2>(),
    )
}

#[proc_macro]
pub fn proof_assert(assertion: TS1) -> TS1 {
    let assertion = parse_macro_input!(assertion with TBlock::parse_within);
    let body = assertion
        .into_iter()
        .map(|ts| {
            let sp = ts.span();
            let stmt = ts.encode();
            quote_spanned! { sp => #stmt }
        })
        .collect::<TS2>();
    TS1::from(quote! {
        {
            #[allow(unused_must_use, unused_variables)]
            let _ = {
                #[rml::spec::assert]
                || {
                    let b: bool = {
                        #body
                    }
                    b
                }
            }
        }
    })
}

fn generate_unique_ident(prefix: &str) -> Ident {
    let uuid = uuid::Uuid::new_v4();
    let ident = format!("{}_{}", prefix, uuid).replace('-', "_");

    Ident::new(&ident, Span::call_site())
}

fn fn_spec_item(spec_id: Ident, sig: Signature, result: FnArg, mut spec: Spec, span: Span) -> TS2 {
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
            let id_str = id.to_string();
            let span = p.span();
            let t = p.encode();
            let mut res: ItemFn = parse_quote_spanned! { span =>
                #[allow(unused_variables, dead_code)]
                #[rml::spec::pre=#id_str]
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
        let id_str = id.to_string();
        let span = p.span();
        let t = p.encode();
        let mut res: ItemFn = parse_quote_spanned! { span =>
            #[allow(unused_variables, dead_code)]
            #[rml::spec::post=#id_str]
            fn #id() -> bool {
                let cond: bool = !!(#t);
                cond
            }
        };
        adapt_sig(&mut res.sig, &post_sig);

        res
    });
    let post_strs = post_idents.iter().map(|i| i.to_string());

    // modifies
    let locset = if let Some(ls) = spec.modifies {
        ls
    } else {
        let mut_params = get_mut_ref_params(&sig);
        let mut locsets: Vec<LocSet> = mut_params
            .map(|a| {
                let asp = a.span();
                match a {
                    FnArg::Receiver(_) => {
                        parse_quote_spanned! { asp => self.* }
                    }
                    FnArg::Typed(t) => {
                        let base = &t.pat;
                        parse_quote_spanned! { asp => #base.* }
                    }
                }
            })
            .collect();
        let ls = if locsets.is_empty() {
            LocSet::Nothing(parse_quote_spanned! { span => nothing })
        } else {
            let first = locsets.remove(0);
            let mut p = Punctuated::new();
            p.push_value(first);
            p.push_punct(Token![,](span));

            LocSet::Group(LocSetGroup { items: p })
        };
        ls
    };
    let modi_id = generate_unique_ident("spec_part_modi");
    let modi_id_str = modi_id.to_string();
    let lse = locset.encode();
    let modi_attr: Attribute =
        parse_quote_spanned! { span => #[rml::spec_part_mod_ref=#modi_id_str] };
    let mut modi: ItemFn = parse_quote_spanned! { span =>
        #[allow(unused_variables, dead_code)]
        #[rml::spec::modi=#modi_id_str]
        fn #modi_id() -> ::rml_contracts::logic::LocSet {
            #lse
        }
    };
    adapt_sig(&mut modi.sig, &sig);

    // variant
    let (var_attr, var) = if let Some(v) = spec.variant {
        let span = v.span();
        let t = v.encode();
        let id = generate_unique_ident("spec_part_var");
        let id_str = id.to_string();
        let mut item: ItemFn = parse_quote_spanned! { span =>
            #[allow(unused_variables, dead_code)]
            #[rml::spec::var=#id_str]
            fn #id() -> impl ::rml_contracts::WellFounded {
                #t
            }
        };
        adapt_sig(&mut item.sig, &sig);
        let id_str = id.to_string();
        let var_attr: Attribute =
            parse_quote_spanned! { span => #[rml::spec_part_var_ref=#id_str] };
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
        let id_str = id.to_string();
        let span = diverges.span();
        let t = diverges.encode();

        let mut item: ItemFn = parse_quote_spanned! { span =>
            #[allow(unused_variables, dead_code)]
            #[rml::spec::div=#id_str]
            fn #id() -> bool {
                let b: bool = !!(#t);
                b
            }
        };
        adapt_sig(&mut item.sig, &sig);
        let id_str = id.to_string();
        let attr: Attribute = parse_quote_spanned! { span => #[rml::spec_part_div_ref=#id_str] };
        (item, attr)
    };

    // name
    let spec_name: Option<Attribute> = spec
        .name
        .map(|n| parse_quote_spanned! { span => #[rml::spec_name=#n] });

    let spec_name_str = spec_id.to_string();
    quote_spanned! { span =>
        #(#pre)*
        #(#post)*
        #div
        #modi
        #var
        #[allow(unused_must_use, unused_variables, dead_code)]
        #[rml::#spec_attr=#spec_name_str]
        #(#[rml::spec_part_pre_ref=#pre_strs])*
        #(#[rml::spec_part_post_ref=#post_strs])*
        #var_attr
        #modi_attr
        #div_attr
        #spec_name
        const #spec_id: bool = false;
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

fn transform_loop(r#loop: LoopKind) -> (Vec<Attribute>, Vec<Stmt>, Expr) {
    match r#loop {
        LoopKind::ForLoop(_) => todo!(),
        LoopKind::Loop(l) => (vec![], vec![], Expr::Loop(l)),
        LoopKind::While(l) => (vec![], vec![], Expr::While(l)),
    }
}

fn loop_var_closure(term: Term, name: &str) -> Stmt {
    let sp = term.span();
    let e = term.encode();
    parse_quote_spanned! { sp =>
        #[allow(unused_must_use)]
        let _ = #[rml::spec::loop_variant = #name]
        || {
            let _ = ::rml_contracts::well_founded_check(#e);
            #e
        };
    }
}

fn loop_inv_closure(term: Term, name: &str) -> Stmt {
    let sp = term.span();
    let e = term.encode();
    parse_quote_spanned! { sp =>
        #[allow(unused_must_use)]
        let _ = #[rml::spec::loop_invariant = #name]
        || {
            let b: bool = #e;
            b
        };
    }
}

fn loop_inv(term: Term, mut r#loop: LoopKind) -> (TS2, TS2, TS2) {
    let sp = term.span();
    let lsp = r#loop.span();
    let first_ident = generate_unique_ident("loop_inv").to_string();
    let mut attrs: Vec<Attribute> =
        vec![parse_quote_spanned! {sp => #[rml::loop_inv_ref = #first_ident]}];
    let mut fns: Vec<Stmt> = vec![loop_inv_closure(term, &first_ident)];

    // TODO: use drain_filter when it becomes available
    let inv_attrs: Vec<_> = r#loop
        .attrs_mut()
        .extract_if(|a| {
            a.path()
                .get_ident()
                .map(|i| i == "invariant")
                .unwrap_or(false)
        })
        .collect();
    let var_attrs: Vec<_> = r#loop
        .attrs_mut()
        .extract_if(|a| {
            a.path()
                .get_ident()
                .map(|i| i == "variant")
                .unwrap_or(false)
        })
        .collect();

    if var_attrs.len() > 1 {
        let attr = &var_attrs[1];
        // TODO: Better error reporting
        panic!(
            "Found {} variants for a loop: {}",
            var_attrs.len(),
            attr.to_token_stream()
        )
    }

    for inv in inv_attrs {
        if let Meta::List(l) = inv.meta {
            let term: Term = syn::parse2(l.tokens).unwrap();
            let name = generate_unique_ident("loop_inv").to_string();
            fns.push(loop_inv_closure(term, &name));
            attrs.push(parse_quote_spanned! {sp => #[rml::loop_inv_ref = #name]});
        } else {
            panic!()
        }
    }

    for var in var_attrs {
        if let Meta::List(l) = var.meta {
            let term: Term = syn::parse2(l.tokens).unwrap();
            let name = generate_unique_ident("loop_var").to_string();
            fns.push(loop_var_closure(term, &name));
            attrs.push(parse_quote_spanned! {sp => #[rml::loop_var_ref = #name]});
        } else {
            panic!()
        }
    }

    let (mut add_attrs, mut add_fns, loop_expr) = transform_loop(r#loop);

    attrs.append(&mut add_attrs);
    fns.append(&mut add_fns);

    (
        quote_spanned! { sp =>
            #(#attrs)*
        },
        quote_spanned! { sp =>
            #(#fns)*
        },
        quote_spanned! { lsp =>
            #loop_expr
        },
    )
}
