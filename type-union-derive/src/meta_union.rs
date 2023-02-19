use proc_macro2::Span;
use quote::{format_ident, quote};

use crate::input::TypeUnionDefinition;
use crate::utils::Generator;

// TODO: finish initial draft for #16

pub struct MetaTypeUnion {
    name: syn::Ident,
    declaration: TypeUnionDefinition,
}

impl TryFrom<TypeUnionDefinition> for MetaTypeUnion {
    type Error = syn::Error;

    fn try_from(declaration: TypeUnionDefinition) -> Result<Self, Self::Error> {
        let width = declaration.type_union().iter_types().count();

        if width < Self::MIN {
            return Err(syn::Error::new(
                declaration.type_union().span(),
                format!("TypeUnion must have at least {} types", Self::MIN),
            ));
        }

        Ok(Self {
            name: syn::Ident::new(&format!("TypeUnion{}", width), Span::call_site()),
            declaration,
        })
    }
}

impl MetaTypeUnion {
    const MIN: usize = 2;

    #[must_use]
    pub fn order(&self) -> usize {
        self.declaration.type_union().iter_types().count()
    }

    #[must_use]
    pub fn declaration(&self) -> proc_macro2::TokenStream {
        let name = &self.name;
        let enum_name = format_ident!("Internal{}", name);

        let generics = Generator::default().take(self.order()).collect::<Vec<_>>();
        let variants = generics.clone().into_iter().map(|g| quote!( #g ( #g ) ));

        quote! {
            enum #enum_name < #(#generics),* > {
                #(#variants , )*
            }

            pub struct #name < #( #generics ),* > ( #enum_name < #( #generics ),* > );
        }
    }
}
