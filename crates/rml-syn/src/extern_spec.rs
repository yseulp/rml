use std::mem;

use proc_macro2::Span;
use syn::{
    punctuated::Punctuated,
    spanned::Spanned,
    token::{self},
    Error, ExprPath, Generics, Ident, Path, PathArguments, PathSegment, Result, Signature, Token,
    Type, TypeParamBound,
};

use crate::attrs::{AttributeInvariant, FnAttribute};

pub mod parsing;
pub mod printing;

pub enum ExternSpecItem {
    Enum(ExternSpecEnum),
    Fn(ExternSpecFn),
    Impl(ExternSpecImpl),
    Mod(ExternSpecMod),
    Struct(ExternSpecStruct),
    Trait(ExternSpecTrait),
}

pub struct ExternSpecEnum {
    pub attrs: Vec<AttributeInvariant>,
    pub enum_token: Token![enum],
    pub ident: Ident,
    pub generics: Generics,
    pub brace_token: token::Brace,
}

pub struct ExternSpecFn {
    pub attrs: Vec<FnAttribute>,
    pub sig: Signature,
    pub semi_token: Token![;],
}

pub struct ExternSpecImpl {
    pub impl_token: Token![impl],
    pub generics: Generics,
    /// Trait this impl implements.
    pub trait_: Option<(Option<Token![!]>, Path, Token![for])>,
    /// The Self type of the impl.
    pub self_ty: Box<Type>,
    pub brace_token: token::Brace,
    pub items: Vec<ExternSpecFn>,
}

pub struct ExternSpecMod {
    pub mod_token: Token![mod],
    pub ident: Ident,
    pub brace_token: token::Brace,
    pub content: Vec<ExternSpecItem>,
}

pub struct ExternSpecStruct {
    pub attrs: Vec<AttributeInvariant>,
    pub struct_token: Token![struct],
    pub ident: Ident,
    pub generics: Generics,
    pub brace_token: token::Brace,
}

pub struct ExternSpecTrait {
    pub attrs: Vec<AttributeInvariant>,
    pub trait_token: Token![trait],
    pub ident: Ident,
    pub generics: Generics,
    pub colon_token: Option<Token![:]>,
    pub supertraits: Punctuated<TypeParamBound, Token![+]>,
    pub brace_token: token::Brace,
    pub items: Vec<ExternSpecFn>,
}

#[derive(Debug, Clone)]
pub enum FnContext {
    None,
    Trait(Ident, Generics),
    Impl(Type),
    TraitImpl { target_ty: Type, trait_: Path },
}

pub enum FlattenedExternSpec {
    Struct(FlattenedStructSpec),
    Enum(FlattenedEnumSpec),
    Trait(FlattenedTraitSpec),
    Fn(FlattenedFnSpec),
}

pub struct FlattenedStructSpec {
    pub span: Span,
    pub attrs: Vec<AttributeInvariant>,
    pub prefix: ExprPath,
    pub ident: Ident,
    pub generics: Generics,
}

pub struct FlattenedEnumSpec {
    pub span: Span,
    pub attrs: Vec<AttributeInvariant>,
    pub prefix: ExprPath,
    pub ident: Ident,
    pub generics: Generics,
}

pub struct FlattenedTraitSpec {
    pub span: Span,
    pub attrs: Vec<AttributeInvariant>,
    pub prefix: ExprPath,
    pub ident: Ident,
    pub generics: Generics,
}

pub struct FlattenedFnSpec {
    pub span: Span,
    pub attrs: Vec<FnAttribute>,
    pub prefix: ExprPath,
    pub sig: Signature,
    pub ctxt: FnContext,
}

pub enum ExternSpecAttributes {
    Invariant(Vec<AttributeInvariant>),
    Fn(Vec<FnAttribute>),
}

impl ExternSpecAttributes {
    pub fn is_empty(&self) -> bool {
        match self {
            ExternSpecAttributes::Invariant(i) => i.is_empty(),
            ExternSpecAttributes::Fn(i) => i.is_empty(),
        }
    }
}

impl ExternSpecItem {
    pub fn replace_inv_attrs(&mut self, new: Vec<AttributeInvariant>) -> Vec<AttributeInvariant> {
        match self {
            Self::Enum(ExternSpecEnum { attrs, .. })
            | Self::Struct(ExternSpecStruct { attrs, .. })
            | Self::Trait(ExternSpecTrait { attrs, .. }) => mem::replace(attrs, new),
            _ => Vec::new(),
        }
    }

    pub fn replace_fn_attrs(&mut self, new: Vec<FnAttribute>) -> Vec<FnAttribute> {
        match self {
            Self::Fn(ExternSpecFn { attrs, .. }) => mem::replace(attrs, new),
            _ => Vec::new(),
        }
    }

    pub fn flatten(self, path: Option<Path>) -> Result<Vec<FlattenedExternSpec>> {
        let prefix = ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: path.unwrap_or_else(|| Path {
                leading_colon: Some(Default::default()),
                segments: Punctuated::new(),
            }),
        };

        let mut flattened = Vec::new();

        flatten_helper(self, prefix, FnContext::None, &mut flattened)?;

        Ok(flattened)
    }
}

fn flatten_helper(
    spec: ExternSpecItem,
    mut prefix: ExprPath,
    fn_ctxt: FnContext,
    flattened: &mut Vec<FlattenedExternSpec>,
) -> Result<()> {
    match spec {
        ExternSpecItem::Enum(e) => flattened.push(FlattenedExternSpec::Enum(FlattenedEnumSpec {
            span: e.span(),
            attrs: e.attrs,
            prefix,
            ident: e.ident,
            generics: e.generics,
        })),
        ExternSpecItem::Fn(f) => {
            prefix.path.segments.push(PathSegment {
                ident: f.sig.ident.clone(),
                arguments: PathArguments::None,
            });
            flattened.push(FlattenedExternSpec::Fn(FlattenedFnSpec {
                span: f.span(),
                attrs: f.attrs,
                prefix,
                sig: f.sig,
                ctxt: fn_ctxt,
            }))
        }
        ExternSpecItem::Impl(i) => {
            if prefix.path.segments.is_empty() {
                prefix.qself = Some(syn::QSelf {
                    lt_token: Default::default(),
                    ty: i.self_ty.clone(),
                    position: 0,
                    as_token: None,
                    gt_token: Default::default(),
                });
                prefix.path.leading_colon = Some(Default::default());
            } else if let Type::Path(ty_path) = &*i.self_ty {
                let mut segment = ty_path.path.segments[0].clone();
                if let PathArguments::AngleBracketed(arg) = &mut segment.arguments {
                    arg.colon2_token = Some(Default::default());
                }

                prefix.path.segments.push(segment);
            } else {
                return Err(Error::new(i.span(), "unsupported form of impl"));
            }

            let ctxt = if let Some((_, trait_, _)) = i.trait_ {
                FnContext::TraitImpl {
                    target_ty: *i.self_ty.clone(),
                    trait_,
                }
            } else {
                FnContext::Impl(*i.self_ty.clone())
            };

            for item in i.items {
                flatten_helper(
                    ExternSpecItem::Fn(item),
                    prefix.clone(),
                    ctxt.clone(),
                    flattened,
                )?
            }
        }
        ExternSpecItem::Mod(m) => {
            prefix.path.segments.push(PathSegment {
                ident: m.ident,
                arguments: PathArguments::None,
            });

            for item in m.content {
                flatten_helper(item, prefix.clone(), FnContext::None, flattened)?;
            }
        }
        ExternSpecItem::Struct(s) => {
            flattened.push(FlattenedExternSpec::Struct(FlattenedStructSpec {
                span: s.span(),
                attrs: s.attrs,
                prefix,
                ident: s.ident,
                generics: s.generics,
            }))
        }
        ExternSpecItem::Trait(t) => {
            flattened.push(FlattenedExternSpec::Trait(FlattenedTraitSpec {
                span: t.span(),
                attrs: t.attrs,
                prefix: prefix.clone(),
                ident: t.ident.clone(),
                generics: t.generics.clone(),
            }));

            prefix.path.segments.push(PathSegment {
                ident: t.ident.clone(),
                arguments: PathArguments::None,
            });

            for item in t.items {
                flatten_helper(
                    ExternSpecItem::Fn(item),
                    prefix.clone(),
                    FnContext::Trait(t.ident.clone(), t.generics.clone()),
                    flattened,
                )?
            }
        }
    }
    Ok(())
}
