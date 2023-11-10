use syn::{
    braced, parenthesized, parse, parse::Parse, punctuated::Punctuated, token, Error, FnArg,
    Result, Signature, Token, Type, TypePath,
};

use super::{
    ExternSpecAttributes, ExternSpecEnum, ExternSpecFn, ExternSpecImpl, ExternSpecItem,
    ExternSpecMod, ExternSpecStruct, ExternSpecTrait,
};
use crate::attrs::{parse_attrs, AttributeInvariant, FnAttribute};

impl Parse for ExternSpecAttributes {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let fork = input.fork();
        let mut fn_err = match parse_attrs::<FnAttribute>(&fork) {
            Ok(a) => return Ok(ExternSpecAttributes::Fn(a)),
            Err(e) => e,
        };
        let inv_err = match parse_attrs::<AttributeInvariant>(input) {
            Ok(a) => return Ok(ExternSpecAttributes::Invariant(a)),
            Err(e) => e,
        };
        fn_err.combine(inv_err);
        Err(fn_err)
    }
}

impl Parse for ExternSpecItem {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = input.parse::<ExternSpecAttributes>()?;
        let lh = input.lookahead1();
        let mut i = if lh.peek(Token![enum]) {
            Self::Enum(input.parse()?)
        } else if lh.peek(Token![fn]) {
            Self::Fn(input.parse()?)
        } else if lh.peek(Token![impl]) {
            Self::Impl(input.parse()?)
        } else if lh.peek(Token![mod]) {
            Self::Mod(input.parse()?)
        } else if lh.peek(Token![struct]) {
            Self::Struct(input.parse()?)
        } else if lh.peek(Token![trait]) {
            Self::Trait(input.parse()?)
        } else {
            return Err(lh.error());
        };

        if !attrs.is_empty() {
            match (attrs, &mut i) {
                (ExternSpecAttributes::Fn(mut attrs), i @ Self::Fn(..)) => {
                    attrs.extend(i.replace_fn_attrs(Vec::new()));
                    i.replace_fn_attrs(attrs);
                }
                (ExternSpecAttributes::Fn(_), i) => {
                    return Err(syn::Error::new_spanned(i, "Expected function"));
                }
                (
                    ExternSpecAttributes::Invariant(mut attrs),
                    i @ (Self::Enum(..) | Self::Struct(..) | Self::Trait(..)),
                ) => {
                    attrs.extend(i.replace_inv_attrs(Vec::new()));
                    i.replace_inv_attrs(attrs);
                }
                (ExternSpecAttributes::Invariant(_), i) => {
                    return Err(syn::Error::new_spanned(
                        i,
                        "Expected enum, struct, or trait",
                    ));
                }
            }
        }

        Ok(i)
    }
}

impl Parse for ExternSpecEnum {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = parse_attrs(input)?;
        let enum_token = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let content;
        let brace_token = braced!(content in input);
        if !content.is_empty() {
            return Err(Error::new(content.span(), "Expected empty braces"));
        }

        Ok(Self {
            attrs,
            enum_token,
            ident,
            generics,
            brace_token,
        })
    }
}

impl Parse for ExternSpecFn {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = parse_attrs(input)?;
        let fn_token = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let content;
        let paren_token = parenthesized!(content in input);
        let inputs = content.parse_terminated(FnArg::parse, Token![,])?;
        let output = input.parse()?;

        let sig = Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token,
            ident,
            generics,
            paren_token,
            inputs,
            variadic: None,
            output,
        };
        let semi_token = input.parse()?;
        Ok(Self {
            attrs,
            sig,
            semi_token,
        })
    }
}

impl Parse for ExternSpecImpl {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let impl_token = input.parse()?;
        let generics = input.parse()?;

        let polarity = if input.peek(Token![!]) && !input.peek2(token::Brace) {
            Some(input.parse::<Token![!]>()?)
        } else {
            None
        };

        let mut first_ty: Type = input.parse()?;
        let self_ty: Type;
        let trait_;
        let is_impl_for = input.peek(Token![for]);

        if is_impl_for {
            let for_token: Token![for] = input.parse()?;
            let mut first_ty_ref = &first_ty;
            while let Type::Group(ty) = first_ty_ref {
                first_ty_ref = &ty.elem;
            }
            if let Type::Path(TypePath { qself: None, .. }) = first_ty_ref {
                while let Type::Group(ty) = first_ty {
                    first_ty = *ty.elem;
                }
                if let Type::Path(TypePath { qself: None, path }) = first_ty {
                    trait_ = Some((polarity, path, for_token));
                } else {
                    unreachable!();
                }
            } else {
                return Err(Error::new_spanned(first_ty_ref, "expected trait path"));
            }
            self_ty = input.parse()?;
        } else {
            trait_ = None;
            self_ty = if let Some(p) = polarity {
                return Err(Error::new_spanned(p, "Unexpected !"));
            } else {
                first_ty
            };
        }

        let content;
        let brace_token = braced!(content in input);

        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        Ok(Self {
            impl_token,
            generics,
            trait_,
            self_ty: Box::new(self_ty),
            brace_token,
            items,
        })
    }
}

impl Parse for ExternSpecMod {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let mod_token = input.parse()?;
        let ident = input.parse()?;
        let c;
        let brace_token = braced!(c in input);
        let mut content = Vec::new();

        while !c.is_empty() {
            content.push(c.parse()?);
        }

        Ok(Self {
            mod_token,
            ident,
            brace_token,
            content,
        })
    }
}

impl Parse for ExternSpecStruct {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = parse_attrs(input)?;
        let struct_token = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let content;
        let brace_token = braced!(content in input);
        if !content.is_empty() {
            return Err(Error::new(content.span(), "Expected empty braces"));
        }

        Ok(Self {
            attrs,
            struct_token,
            ident,
            generics,
            brace_token,
        })
    }
}

impl Parse for ExternSpecTrait {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = parse_attrs(input)?;
        let trait_token = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;

        let colon_token: Option<Token![:]> = input.parse()?;

        let mut supertraits = Punctuated::new();
        if colon_token.is_some() {
            loop {
                if input.peek(token::Brace) {
                    break;
                }
                supertraits.push_value(input.parse()?);
                if input.peek(token::Brace) {
                    break;
                }
                supertraits.push_punct(input.parse()?);
            }
        }

        let content;
        let brace_token = braced!(content in input);
        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        Ok(Self {
            attrs,
            trait_token,
            ident,
            generics,
            colon_token,
            supertraits,
            brace_token,
            items,
        })
    }
}
