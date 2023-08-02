use syn::{FnArg, PatType, Receiver, Signature, Type, TypeReference};

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
