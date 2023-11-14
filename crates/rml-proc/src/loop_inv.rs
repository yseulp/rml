use proc_macro2::TokenStream as TS2;
use quote::{quote_spanned, ToTokens};
use rml_syn::{Encode, LocSet, Term};
use syn::{
    parse::Parse, parse_quote_spanned, spanned::Spanned, Attribute, Expr, Ident, Meta, Result, Stmt,
};

use crate::{gen_unique_ident, subject::LoopKind, util::extract_attrs};

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
        let _ = #[rml::spec::loop_var = #name]
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
        let _ = #[rml::spec::loop_inv = #name]
        || {
            let b: bool = #e;
            b
        };
    }
}

fn loop_mod_closure(ls: LocSet, name: &str) -> Stmt {
    let sp = ls.span();
    let e = ls.encode();
    parse_quote_spanned! { sp =>
        #[allow(unused_must_use)]
        let _ = #[rml::spec::loop_modi = #name]
        || {
            let l: ::rml_contracts::logic::LocSet = #e;
            l
        };
    }
}

/// Extract attributes of the loop with the path `attr`.
#[inline]
fn extract_loop_attrs(r#loop: &mut LoopKind, attr: &'static str) -> Vec<Attribute> {
    extract_attrs(r#loop.attrs_mut(), attr)
}

/// Collects the closures and attributes for one part of the specification such
/// as invariants.
///
/// - `fns`: Collection of the closures.
/// - `attrs`: Collection of the attributes.
/// - `to_parse`: The attributes the contents of which we need to parse, e.g.,
///   invariant attributes.
/// - `f`: A function to create the closure.
/// - `prefix`: Prefix of the closure's name.
/// - `attr_name`: Name of the generated attribute.
fn add_closures_and_attrs<F, T>(
    fns: &mut Vec<Stmt>,
    attrs: &mut Vec<Attribute>,
    to_parse: Vec<Attribute>,
    f: F,
    prefix: &str,
    attr_name: &'static str,
) -> Result<()>
where
    F: Fn(T, &str) -> Stmt,
    T: Parse,
{
    for a in to_parse {
        let sp = a.span();
        if let Meta::List(l) = a.meta {
            let t: T = syn::parse2(l.tokens)?;
            let name = gen_unique_ident(prefix).to_string();
            fns.push(f(t, &name));
            let attr_ident = Ident::new(attr_name, sp);
            attrs.push(parse_quote_spanned! {sp => #[rml::#attr_ident = #name]});
        } else {
            panic!()
        }
    }
    Ok(())
}

/// Generate the necessary code for specifying a loop. `term` is the first
/// invariant. `r#loop` is the loop.
///
/// Returns a thruple of [TokenStream]s:
/// 1. The specification attributes
/// 2. The spec closures.
/// 3. The transformed loop.
///
/// [TokenStream]: TS2
pub(crate) fn loop_inv(term: Term, mut r#loop: LoopKind) -> Result<(TS2, TS2, TS2)> {
    let sp = term.span();
    let lsp = r#loop.span();
    let first_ident = gen_unique_ident("loop_inv").to_string();
    let mut attrs: Vec<Attribute> =
        vec![parse_quote_spanned! {sp => #[rml::loop_inv_ref = #first_ident]}];
    let mut fns: Vec<Stmt> = vec![loop_inv_closure(term, &first_ident)];

    let inv_attrs = extract_loop_attrs(&mut r#loop, "invariant");
    let var_attrs = extract_loop_attrs(&mut r#loop, "variant");
    let mod_attrs = extract_loop_attrs(&mut r#loop, "modifies");

    if var_attrs.len() > 1 {
        let attr = &var_attrs[1];
        syn::Error::new(
            attr.span(),
            format!(
                "Found {} variants for a loop: {}",
                var_attrs.len(),
                attr.to_token_stream()
            ),
        );
    }

    if mod_attrs.len() > 1 {
        let attr = &mod_attrs[1];
        syn::Error::new(
            attr.span(),
            format!(
                "Found {} `modifies` attributes for a loop: {}",
                var_attrs.len(),
                attr.to_token_stream()
            ),
        );
    }

    add_closures_and_attrs(
        &mut fns,
        &mut attrs,
        inv_attrs,
        loop_inv_closure,
        "loop_inv",
        "loop_inv_ref",
    )?;

    add_closures_and_attrs(
        &mut fns,
        &mut attrs,
        mod_attrs,
        loop_mod_closure,
        "loop_mod",
        "loop_modi_ref",
    )?;

    add_closures_and_attrs(
        &mut fns,
        &mut attrs,
        var_attrs,
        loop_var_closure,
        "loop_var",
        "loop_var_ref",
    )?;

    let (mut add_attrs, mut add_fns, loop_expr) = transform_loop(r#loop);

    attrs.append(&mut add_attrs);
    fns.append(&mut add_fns);

    Ok((
        quote_spanned! { sp =>
            #(#attrs)*
        },
        quote_spanned! { sp =>
            #(#fns)*
        },
        quote_spanned! { lsp =>
            #loop_expr
        },
    ))
}
