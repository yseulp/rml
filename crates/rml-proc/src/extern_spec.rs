use proc_macro::TokenStream as TS1;
use proc_macro2::{Span, TokenStream as TS2};
use quote::{quote, quote_spanned};
use rml_syn::{
    extern_spec::ExternSpecItem, subject::LogicSubject, Encode, SpecContent, TBlock, Term,
};

use syn::{parse_macro_input, parse_quote, spanned::Spanned, Path, ReturnType};

pub(crate) fn extern_spec(subject: ExternSpecItem, path: Option<Path>) -> TS2 {
    quote! {}
}
