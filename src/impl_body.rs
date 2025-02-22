// use proc_macro::TokenStream;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DataStruct, Fields, FieldsNamed, Type};

pub fn body(ast: &syn::DeriveInput) -> TokenStream {
    let ident = &ast.ident.clone();
    let data = &ast.data;

    let fields = match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => named,
        _ => panic!("it should be structure"),
    };
    let mut idents = Vec::new();
    let mut tys = Vec::new();
    let mut attributes = Vec::new();
    let mut visibilities = Vec::new();
    let mut mutabilities = Vec::new();
    let mut colon_tokens = Vec::new();

    for x in fields.into_iter() {
        attributes.push(x.attrs.clone());
        visibilities.push(x.vis.clone());
        mutabilities.push(x.mutability.clone());
        idents.push(x.ident.clone().unwrap());
        colon_tokens.push(x.colon_token);
        tys.push(x.ty.clone());
    }

    fn ty_is_outer_type(f: &syn::Field, outer_type: &str) -> bool {
        if let &Type::Path(syn::TypePath {
            path: syn::Path { ref segments, .. },
            ..
        }) = &f.ty
        {
            if &segments[0].ident.to_string() == outer_type {
                return true;
            }
        }
        false
    }

    let builder_ident = format_ident!("{}builder", ident);
    let new_struct = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if ty_is_outer_type(&f, "Option") {
            return quote! {#name: #ty,};
        }
        return quote! {#name: std::option::Option<#ty>,};
    });
    let gen_struct = quote! {
        pub struct #builder_ident {
            #(#new_struct)*
        }
    };

    fn get_field_attribute(
        attributes: &Vec<syn::Attribute>,
        literal_vec: &Vec<String>,
        attri_ident: &str,
        token_ident: &str,
        punct_char: char,
    ) -> (Option<String>, Option<proc_macro2::TokenStream>) {
        let meta = &attributes[0].meta;

        if let syn::Meta::List(nvm) = meta {
            let syn::MetaList { path, tokens, .. } = nvm;

            if path.is_ident(attri_ident) {
                //"builder"
                let mut tokens_iterator = tokens.clone().into_iter();
                let expect_each = tokens_iterator.next().unwrap();
                if let proc_macro2::TokenTree::Ident(ref ident) = expect_each {
                    if ident != token_ident {
                        return (
                            std::option::Option::None,
                            std::option::Option::Some(
                                syn::Error::new_spanned(nvm, "expected `builder(each =  \"...\")`")
                                    .to_compile_error(),
                            ),
                            // Some(compile_error!("This macro only accepts `foo` or `bar`")),
                        );
                    } else {
                        //"each"
                        let expect_punct = tokens_iterator.next().unwrap();
                        if let proc_macro2::TokenTree::Punct(punct) = expect_punct {
                            if punct.as_char() == punct_char {
                                // '='
                                let expect_literal = tokens_iterator.next().unwrap();
                                if let proc_macro2::TokenTree::Literal(literal) = expect_literal {
                                    if let syn::Lit::Str(litstr) = syn::Lit::new(literal) {
                                        if literal_vec.contains(&litstr.value().to_string()) {
                                            return (
                                                Some(litstr.value()),
                                                std::option::Option::None,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        };

        panic!("...");
    }

    let initial_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let attribute = &f.attrs;

        if attribute.len() > 0 && ty_is_outer_type(f, "Vec") {
            return quote! {
                #name: Some(Vec::new()),
            };
        }

        return quote! {
            #name: std::option::Option::None,
        };
    });

    let gen_builder = quote! {
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#initial_fields)*
                }
            }
        }

    };

    fn get_inner_type(f: &syn::Field) -> &syn::Type {
        if let Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) = &f.ty
        {
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args: ref b,
                ..
            }) = segments[0].arguments
            {
                if let syn::GenericArgument::Type(ref inner_type) = b[0] {
                    return inner_type;
                }
            }
        }
        return &f.ty;
    }

    let set_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let attribute = &f.attrs;
        let literal_vec = vec![String::from("env"), String::from("arg")];

        let newty = if ty_is_outer_type(f, "Option") {
            get_inner_type(f)
        } else {
            &f.ty
        };

        let output = if attribute.len() > 0 && ty_is_outer_type(f, "Vec") {
            let inner_type_vec = get_inner_type(f);

            let (literal_name, macro_error) =
                get_field_attribute(attribute, &literal_vec, "builder", "each", '=');

            if macro_error.is_some() {
                return macro_error.unwrap();
            } else {
                let attribute_name =
                    syn::Ident::new(&literal_name.unwrap(), name.clone().unwrap().span());

                quote! {
                    fn #attribute_name(&mut self, #attribute_name: #inner_type_vec) -> &mut Self  {
                        if self.#name.is_some() {
                            self.#name.as_mut().unwrap().push(#attribute_name);
                        } else {
                            self.#name = Some(vec![#attribute_name]);
                        }
                        self
                    }
                }
            }
        } else {
            quote! {
                fn #name(&mut self, #name: #newty) -> &mut Self  {
                    self.#name = Some(#name);
                    self
                }

            }
        };
        output
    });

    let gen_set_fields = quote! {
        impl #builder_ident {
           #(#set_fields)*
        }
    };

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;

        if ty_is_outer_type(&f, "Option") {
            return quote! {#name: self.#name.clone(),};
        }
        quote! {#name: self.#name.as_ref().ok_or("incorrect")?.clone(),}
    });

    let gen_build = quote! {
        impl #builder_ident {
            pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>>{
                Ok(#ident{#(#build_fields)*})

            }
        }
    };

    quote! {
        #gen_struct
        #gen_builder
        #gen_set_fields
        #gen_build
    }
}
