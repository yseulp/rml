use proc_macro2::Span;
use syn::{FnArg, Ident, PatType, Receiver, Signature, Type, TypeReference};

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

pub(crate) fn generate_unique_ident(prefix: &str) -> Ident {
    let uuid = uuid::Uuid::new_v4();
    let ident = format!("{}_{}", prefix, uuid).replace('-', "_");

    Ident::new(&ident, Span::call_site())
}
