mod impl_body;
use proc_macro::TokenStream;

// #[proc_macro_derive(Builder, attributes(builder))]
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    impl_body::body(&ast).into()
}
