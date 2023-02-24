use std::fmt;

use indexmap::{IndexMap, IndexSet};
use quote::ToTokens;

use crate::impl_declaration::{GenericType, Variadic};

#[derive(Clone, PartialEq, Eq)]
pub struct TypeMapping {
    generic_types: IndexMap<syn::Type, syn::Type>,
    variadic_types: IndexMap<Variadic, IndexSet<syn::Type>>,
}

impl TypeMapping {
    pub fn new() -> Self {
        Self {
            generic_types: IndexMap::new(),
            variadic_types: IndexMap::new(),
        }
    }

    pub fn iter_variadics(&self) -> impl Iterator<Item = (&Variadic, &IndexSet<syn::Type>)> {
        self.variadic_types.iter()
    }

    pub fn add_generic_type(&mut self, generic_type: GenericType, ty: syn::Type) {
        self.generic_types.insert(generic_type.to_type(), ty);
    }

    pub fn add_variadic_type(&mut self, variadic_type: Variadic, ty: IndexSet<syn::Type>) {
        self.variadic_types
            .entry(variadic_type)
            .or_default()
            .extend(ty);
    }

    pub fn get(&self, ty: &syn::Type) -> Option<&syn::Type> {
        self.generic_types.get(ty)
    }

    pub fn get_variadic(&self, variadic: &Variadic) -> Option<&IndexSet<syn::Type>> {
        self.variadic_types.get(variadic)
    }
}

struct DebugPath<'a>(&'a syn::Path);

impl fmt::Debug for DebugPath<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Path({})", &self.0.to_token_stream().to_string())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct DebugType<'a>(&'a syn::Type);

impl fmt::Debug for DebugType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            syn::Type::Path(path) => write!(f, "Path({})", &path.to_token_stream().to_string()),
            _ => f
                .debug_tuple("Type")
                .field(&self.0.into_token_stream().to_string())
                .finish(),
        }
    }
}

struct DebugIter<I> {
    iter: I,
}

impl<I> fmt::Debug for DebugIter<I>
where
    I: Iterator + Clone,
    <I as Iterator>::Item: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter.clone()).finish()
    }
}

impl fmt::Debug for TypeMapping {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeMapping")
            .field(
                "generic_types",
                &DebugIter {
                    iter: self
                        .generic_types
                        .iter()
                        .map(|(k, v)| (DebugType(k), DebugType(v))),
                },
            )
            .field(
                "variadic_types",
                &DebugIter {
                    iter: self
                        .variadic_types
                        .iter()
                        // TODO: do not collect into IndexSet?
                        .map(|(k, v)| (k, v.into_iter().map(DebugType).collect::<IndexSet<_>>())),
                },
            )
            .finish()
    }
}
