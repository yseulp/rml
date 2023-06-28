use proc_macro::TokenStream;
use rml_syn::Spec;
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn spec(attr: TokenStream, item: TokenStream) -> TokenStream {
    let sp = parse_macro_input!(attr as Spec);
    println!("{:#?}", sp);
    item
}

#[proc_macro_attribute]
pub fn requires(_attr: TokenStream, _item: TokenStream) -> TokenStream {
    todo!()
}
