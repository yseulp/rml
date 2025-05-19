//! External RML specification for items outside the current crate. Since the
//! source code is not available, attributes are not available and compiled
//! away. Hence, developers must add specification in a different way.

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

mod parsing;
mod printing;

/// An external specification. Allows specifying functions and data structures
/// outside of the current crate, i.e., when no source code is available.
///
/// Includes
/// - Invariants for enum/struct/trait,
/// - Function specifications,
/// - Modules to give structure to external specifications.
pub enum ExternSpecItem {
    /// An enum declaration and its invariant.
    Enum(ExternSpecEnum),
    /// A function declaration and its specification.
    Fn(ExternSpecFn),
    /// An implementation of a data structure.
    ///
    /// Allows specification of the implied functions.
    Impl(ExternSpecImpl),
    /// A module.
    ///
    /// Allows structuring external specifications.
    Mod(ExternSpecMod),
    /// A struct declaration and its invariant.
    Struct(ExternSpecStruct),
    /// A trait declaration.
    ///
    /// Allows invariants and specification of its methods.
    Trait(ExternSpecTrait),
}

/// An enum declaration and its invariant.
pub struct ExternSpecEnum {
    /// Invarint attributes.
    pub attrs: Vec<AttributeInvariant>,
    pub enum_token: Token![enum],
    pub ident: Ident,
    pub generics: Generics,
    /// A brace is required by Rust's syntax but must be empty.
    pub brace_token: token::Brace,
}

/// A function declaration and its specification.
pub struct ExternSpecFn {
    /// Function specification attributes.
    pub attrs: Vec<FnAttribute>,
    pub sig: Signature,
    /// External functions have no body.
    pub semi_token: Token![;],
}

/// An implementation of a data structure.
///
/// Allows specification of the implied functions.
pub struct ExternSpecImpl {
    pub impl_token: Token![impl],
    pub generics: Generics,
    /// Trait this `impl` implements.
    pub trait_: Option<(Option<Token![!]>, Path, Token![for])>,
    /// The Self type of the `impl`.
    pub self_ty: Box<Type>,
    pub brace_token: token::Brace,
    pub items: Vec<ExternSpecFn>,
}

/// A module.
///
/// Allows structuring external specifications.
pub struct ExternSpecMod {
    pub mod_token: Token![mod],
    pub ident: Ident,
    pub brace_token: token::Brace,
    pub content: Vec<ExternSpecItem>,
}

/// A struct declaration and its invariant.
pub struct ExternSpecStruct {
    pub attrs: Vec<AttributeInvariant>,
    pub struct_token: Token![struct],
    pub ident: Ident,
    pub generics: Generics,
    pub brace_token: token::Brace,
}

/// A trait declaration.
///
/// Allows invariants and specification of its methods.
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

/// The context of an external function declaration.
#[derive(Debug, Clone)]
pub enum FnContext {
    /// No special context.
    None,
    /// Function inside a trait declaration.
    Trait(Ident, Generics),
    /// Inside an `impl` of a specific type.
    Impl(Type),
    /// Inside an `impl` of a trait for a type.
    TraitImpl { target_ty: Type, trait_: Path },
}

impl FnContext {
    pub fn self_ty(&self) -> Option<Type> {
        match self {
            FnContext::None => None,
            FnContext::Trait(..) => todo!(),
            FnContext::Impl(ty) => Some(ty.clone()),
            FnContext::TraitImpl { target_ty, .. } => Some(target_ty.clone()),
        }
    }
}

/// The flattened external spec. See [`ExternSpecItem::flatten()`].
pub enum FlattenedExternSpec {
    Struct(FlattenedStructSpec),
    Enum(FlattenedEnumSpec),
    Trait(FlattenedTraitSpec),
    Fn(Box<FlattenedFnSpec>),
}

/// A specification for an external struct.
pub struct FlattenedStructSpec {
    /// Span of the source.
    pub span: Span,
    /// Invariant attributes.
    pub attrs: Vec<AttributeInvariant>,
    /// The path to the struct.
    pub prefix: ExprPath,
    /// Identifier of the struct.
    pub ident: Ident,
    /// Generics of the struct.
    pub generics: Generics,
}

/// A specification for an external enum.
pub struct FlattenedEnumSpec {
    /// Span of the source.
    pub span: Span,
    /// Invariant attributes.
    pub attrs: Vec<AttributeInvariant>,
    /// The path to the enum.
    pub prefix: ExprPath,
    /// Identifier of the enum.
    pub ident: Ident,
    /// Generics of the enum.
    pub generics: Generics,
}

/// A specification for an external trait.
pub struct FlattenedTraitSpec {
    /// Span of the source.
    pub span: Span,
    /// Invariant attributes.
    pub attrs: Vec<AttributeInvariant>,
    /// The path to the trait.
    pub prefix: ExprPath,
    /// Identifier of the trait.
    pub ident: Ident,
    /// Generics of the trait.
    pub generics: Generics,
}

/// A specification for an external function.
pub struct FlattenedFnSpec {
    /// Span of the source.
    pub span: Span,
    /// Function attributes.
    pub attrs: Vec<FnAttribute>,
    /// The path to the function.
    pub prefix: ExprPath,
    /// The function's signature.
    pub sig: Signature,
    /// Context of the function.
    pub ctxt: FnContext,
}

/// Allowed attributes of extern spec items. Only RML attributes are permitted.
pub enum ExternSpecAttributes {
    /// Invariants for data structures and traits.
    Invariant(Vec<AttributeInvariant>),
    /// Function specification.
    Fn(Vec<FnAttribute>),
}

impl ExternSpecAttributes {
    /// Returns `true` if the vector contains no elements.
    pub fn is_empty(&self) -> bool {
        match self {
            ExternSpecAttributes::Invariant(i) => i.is_empty(),
            ExternSpecAttributes::Fn(i) => i.is_empty(),
        }
    }
}

impl ExternSpecItem {
    /// Replace the invariant attributes of the item with `new` and return the
    /// old value.
    ///
    /// If there are no invariant attributes, returns an empty vector.
    pub fn replace_inv_attrs(&mut self, new: Vec<AttributeInvariant>) -> Vec<AttributeInvariant> {
        match self {
            Self::Enum(ExternSpecEnum { attrs, .. })
            | Self::Struct(ExternSpecStruct { attrs, .. })
            | Self::Trait(ExternSpecTrait { attrs, .. }) => mem::replace(attrs, new),
            _ => Vec::new(),
        }
    }

    /// Replace the function attributes of the item with `new` and return the
    /// old value.
    ///
    /// If there are no function attributes, returns an empty vector.
    pub fn replace_fn_attrs(&mut self, new: Vec<FnAttribute>) -> Vec<FnAttribute> {
        match self {
            Self::Fn(ExternSpecFn { attrs, .. }) => mem::replace(attrs, new),
            _ => Vec::new(),
        }
    }

    /// Flatten the external spec item and collect all resulting specs.
    ///
    /// This is necessary because we allow modules, impls and traits in
    /// external specs, which either don't have any specification (`mod`,
    /// `impl`), or can have multiple specs (trait invariant and specified
    /// methods).
    ///
    /// We also discard any tokens, delimiters, etc. and store the original
    /// span instead. The original, nested structure of modules, impls, and
    /// traits is captured by the path `prefix`.
    ///
    /// Additionally, we store the context of specified functions. See
    /// [FlattenedFnSpec].
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

/// Flatten the external spec `spec` item and collect all resulting specs
/// ---as described by [`ExternSpecItem::flatten()`]---in `flattened`.
///
/// The `prefix` is the path to the item, excluding its name.
///
/// `fn_ctxt` stores the current context for functions.
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
            flattened.push(FlattenedExternSpec::Fn(Box::new(FlattenedFnSpec {
                span: f.span(),
                attrs: f.attrs,
                prefix,
                sig: f.sig,
                ctxt: fn_ctxt,
            })))
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
