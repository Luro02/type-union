use syn::punctuated::Punctuated;
use syn::Token;

trait PathArgumentsExt {
    fn len(&self) -> usize;
}

impl PathArgumentsExt for syn::PathArguments {
    fn len(&self) -> usize {
        match self {
            syn::PathArguments::None => 0,
            syn::PathArguments::AngleBracketed(args) => args.args.len(),
            syn::PathArguments::Parenthesized(args) => args.inputs.len(),
        }
    }
}

pub trait Context {
    #[must_use]
    fn is_generic(&self, path: &syn::Path) -> bool;
}

impl<'a, C: Context> Context for &'a C {
    fn is_generic(&self, name: &syn::Path) -> bool {
        (*self).is_generic(name)
    }
}

impl<'a> Context for &'a dyn Context {
    fn is_generic(&self, name: &syn::Path) -> bool {
        (*self).is_generic(name)
    }
}

impl Context for () {
    fn is_generic(&self, _: &syn::Path) -> bool {
        false
    }
}

impl<A: Context, B: Context> Context for (A, B) {
    fn is_generic(&self, name: &syn::Path) -> bool {
        self.0.is_generic(name) || self.1.is_generic(name)
    }
}

impl Context for syn::Generics {
    fn is_generic(&self, path: &syn::Path) -> bool {
        let Some(path_ident) = path.get_ident() else {
            return false;
        };

        self.params.iter().any(|param| {
            if let syn::GenericParam::Type(syn::TypeParam { ident, .. }) = param {
                path_ident == ident
            } else {
                false
            }
        })
    }
}

pub trait LooksLike<T = Self> {
    /// Returns true if the path is equal to the other path.
    ///
    /// This is different from the `PartialEq` implementation, because
    /// paths that are qualified and those that are not are considered equal.
    ///
    /// For example `Copy` and `::core::marker::Copy` are considered equal.
    /// Similarly arguments are not compared for their actual values, but only
    /// for their types ("they look equal").
    #[must_use]
    fn looks_like(&self, other: &T) -> bool {
        self.looks_like_with(other, &())
    }

    #[must_use]
    fn looks_like_with(&self, other: &T, ctx: &dyn Context) -> bool;
}

impl<U, T: LooksLike<U>> LooksLike<U> for &T {
    fn looks_like(&self, other: &U) -> bool {
        (*self).looks_like(other)
    }

    fn looks_like_with(&self, other: &U, ctx: &dyn Context) -> bool {
        (*self).looks_like_with(other, ctx)
    }
}

impl<U, T: LooksLike<U>> LooksLike<Option<U>> for Option<T> {
    fn looks_like(&self, other: &Option<U>) -> bool {
        match (self, other) {
            (Some(left), Some(right)) => left.looks_like(right),
            (None, None) => true,
            _ => false,
        }
    }

    fn looks_like_with(&self, other: &Option<U>, ctx: &dyn Context) -> bool {
        match (self, other) {
            (Some(left), Some(right)) => left.looks_like_with(right, ctx),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<M, N, A: LooksLike<M>, B: LooksLike<N>> LooksLike<(M, N)> for (A, B) {
    fn looks_like(&self, other: &(M, N)) -> bool {
        self.0.looks_like(&other.0) && self.1.looks_like(&other.1)
    }

    fn looks_like_with(&self, other: &(M, N), ctx: &dyn Context) -> bool {
        self.0.looks_like_with(&other.0, ctx) && self.1.looks_like_with(&other.1, ctx)
    }
}

impl<U, T: LooksLike<U>> LooksLike<Box<U>> for Box<T> {
    fn looks_like(&self, other: &Box<U>) -> bool {
        self.as_ref().looks_like(other.as_ref())
    }

    fn looks_like_with(&self, other: &Box<U>, ctx: &dyn Context) -> bool {
        self.as_ref().looks_like_with(other.as_ref(), ctx)
    }
}

impl LooksLike for syn::TypeMacro {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.mac.looks_like_with(&other.mac, ctx)
    }
}

impl LooksLike for syn::Macro {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        // NOTE: tokens are not compared, because they are abitrary?
        self.path.looks_like_with(&other.path, ctx)
    }
}

impl LooksLike for syn::Lifetime {
    fn looks_like_with(&self, _other: &Self, _ctx: &dyn Context) -> bool {
        // lifetimes look always alike
        true
    }
}

impl LooksLike for syn::Expr {
    fn looks_like_with(&self, other: &Self, _ctx: &dyn Context) -> bool {
        // TODO: compare expressions?
        self == other
    }
}

impl LooksLike for syn::Binding {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.ident == other.ident && self.ty.looks_like_with(&other.ty, ctx)
    }
}

impl LooksLike for syn::Constraint {
    fn looks_like_with(&self, other: &Self, _ctx: &dyn Context) -> bool {
        // TODO: compare bounds?
        self.ident == other.ident && self.bounds.len() == other.bounds.len()
    }
}

impl LooksLike for syn::GenericArgument {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        match (self, other) {
            // if one path consists of a single identifier, then it could be a generic
            // and generics are equal to all types
            (Self::Type(syn::Type::Path(path)), Self::Type(other))
            | (Self::Type(other), Self::Type(syn::Type::Path(path)))
                if path.path.get_ident().is_some()
                    // the ctx is needed to ascertain that the ident is actually a generic
                    // otherwise for example u8 could be equal to anything
                    && ctx.is_generic(&path.path)
                    // type union macros can not be in the place of a generic
                    // TODO: does this make sense?
                    && !(matches!(other, syn::Type::Macro(syn::TypeMacro { mac }) if mac.path.looks_like(&syn::parse_quote!(type_union)))) =>
            {
                true
            }
            (Self::Type(left), Self::Type(right)) => left.looks_like_with(right, ctx),
            (Self::Lifetime(left), Self::Lifetime(right)) => left.looks_like_with(right, ctx),
            (Self::Const(left), Self::Const(right)) => left.looks_like_with(right, ctx),
            (Self::Binding(left), Self::Binding(right)) => left.looks_like_with(right, ctx),
            (Self::Constraint(left), Self::Constraint(right)) => left.looks_like_with(right, ctx),
            (_, _) => false,
        }
    }
}

impl LooksLike for syn::TypeArray {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.elem.looks_like_with(&other.elem, ctx) && self.len.looks_like_with(&other.len, ctx)
    }
}

impl LooksLike for syn::TypeGroup {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.elem.looks_like_with(&other.elem, ctx)
    }
}

impl LooksLike for syn::TypeParen {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.elem.looks_like_with(&other.elem, ctx)
    }
}

impl LooksLike for syn::QSelf {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        // TODO: should position be compared?
        self.ty.looks_like_with(&other.ty, ctx)
            && self.as_token.looks_like_with(&other.as_token, ctx)
    }
}

impl LooksLike for syn::TypePath {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.qself.looks_like_with(&other.qself, ctx) && self.path.looks_like_with(&other.path, ctx)
    }
}

impl LooksLike for syn::BoundLifetimes {
    fn looks_like_with(&self, other: &Self, _ctx: &dyn Context) -> bool {
        self.lifetimes.len() == other.lifetimes.len()
    }
}

impl LooksLike for syn::Abi {
    fn looks_like_with(&self, other: &Self, _ctx: &dyn Context) -> bool {
        self == other
    }
}

impl<U, T: LooksLike<U>, P> LooksLike<Punctuated<U, P>> for Punctuated<T, P> {
    fn looks_like_with(&self, other: &Punctuated<U, P>, ctx: &dyn Context) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(left, right)| left.looks_like_with(right, ctx))
    }
}

impl LooksLike for syn::BareFnArg {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.name.is_some() == other.name.is_some() && self.ty.looks_like_with(&other.ty, ctx)
    }
}

impl LooksLike for syn::Variadic {
    fn looks_like_with(&self, other: &Self, _ctx: &dyn Context) -> bool {
        self == other
    }
}

impl LooksLike for syn::ReturnType {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        match (self, other) {
            (Self::Default, Self::Default) => true,
            (Self::Type(_, left), Self::Type(_, right)) => left.looks_like_with(right, ctx),
            (_, _) => false,
        }
    }
}

impl LooksLike for syn::TypeBareFn {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.lifetimes.looks_like_with(&other.lifetimes, ctx)
            && self.unsafety.looks_like_with(&other.unsafety, ctx)
            && self.abi.looks_like_with(&other.abi, ctx)
            && self.inputs.looks_like_with(&other.inputs, ctx)
            && self.variadic.looks_like_with(&other.variadic, ctx)
            && self.output.looks_like_with(&other.output, ctx)
    }
}

impl LooksLike for syn::TraitBound {
    fn looks_like_with(&self, other: &Self, _ctx: &dyn Context) -> bool {
        self == other
    }
}

impl LooksLike for syn::TypeParamBound {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        match (self, other) {
            (Self::Trait(left), Self::Trait(right)) => left.looks_like_with(right, ctx),
            (Self::Lifetime(left), Self::Lifetime(right)) => left.looks_like_with(right, ctx),
            (_, _) => false,
        }
    }
}

impl LooksLike for syn::TypeImplTrait {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.bounds.looks_like_with(&other.bounds, ctx)
    }
}

impl LooksLike for syn::TypePtr {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.const_token.looks_like_with(&other.const_token, ctx)
            && self.mutability.looks_like_with(&other.mutability, ctx)
            && self.elem.looks_like_with(&other.elem, ctx)
    }
}

impl LooksLike for syn::TypeReference {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.lifetime.looks_like_with(&other.lifetime, ctx)
            && self.mutability.looks_like_with(&other.mutability, ctx)
            && self.elem.looks_like_with(&other.elem, ctx)
    }
}

impl LooksLike for syn::TypeSlice {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.elem.looks_like_with(&other.elem, ctx)
    }
}

impl LooksLike for syn::TypeTraitObject {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        // TODO: for this one the order of bounds does not matter
        self.bounds.looks_like_with(&other.bounds, ctx)
    }
}

impl LooksLike for syn::TypeTuple {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        self.elems.looks_like_with(&other.elems, ctx)
    }
}

impl LooksLike for syn::Type {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        match (self, other) {
            (Self::Array(left), Self::Array(right)) => left.looks_like_with(right, ctx),
            (Self::BareFn(left), Self::BareFn(right)) => left.looks_like_with(right, ctx),
            (Self::Group(left), Self::Group(right)) => left.looks_like_with(right, ctx),
            (Self::ImplTrait(left), Self::ImplTrait(right)) => left.looks_like_with(right, ctx),
            // TypeInfer
            (Self::Macro(left), Self::Macro(right)) => left.looks_like_with(right, ctx),
            // Never
            (Self::Paren(left), Self::Paren(right)) => left.looks_like_with(right, ctx),
            (Self::Path(left), Self::Path(right)) => left.looks_like_with(&right, ctx),
            (Self::Ptr(left), Self::Ptr(right)) => left.looks_like_with(right, ctx),
            (Self::Reference(left), Self::Reference(right)) => left.looks_like_with(right, ctx),
            (Self::Slice(left), Self::Slice(right)) => left.looks_like_with(right, ctx),
            (Self::TraitObject(left), Self::TraitObject(right)) => left.looks_like_with(right, ctx),
            (Self::Tuple(left), Self::Tuple(right)) => left.looks_like_with(right, ctx),
            // Verbatim
            (_, _) => self == other,
        }
    }
}

impl LooksLike for syn::AngleBracketedGenericArguments {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        // TODO: two PathSegment will look alike, even if their bounds are in different order?
        self.colon2_token.looks_like_with(&other.colon2_token, ctx)
            && self.args.looks_like_with(&other.args, ctx)
    }
}

impl LooksLike for syn::PathSegment {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        if self.arguments.is_empty() && ctx.is_generic(&syn::Path::from(self.ident.clone())) {
            return true;
        } else if other.arguments.is_empty()
            && ctx.is_generic(&syn::Path::from(other.ident.clone()))
        {
            return true;
        }

        if self.ident != other.ident || self.arguments.len() != other.arguments.len() {
            return false;
        }

        match (&self.arguments, &other.arguments) {
            (syn::PathArguments::None, syn::PathArguments::None) => true,
            (syn::PathArguments::Parenthesized(left), syn::PathArguments::Parenthesized(right)) => {
                left.inputs.len() == right.inputs.len()
            }
            (
                syn::PathArguments::AngleBracketed(left),
                syn::PathArguments::AngleBracketed(right),
            ) => left.looks_like_with(right, ctx),
            _ => false,
        }
    }
}

impl LooksLike for syn::Path {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        if self == other {
            return true;
        }

        // iter in reverse order, so qualified and unqualified paths are considered equal
        // for example `marker::Copy` and `::core::marker::Copy` would be considered equal
        self.segments
            .iter()
            .rev()
            .zip(other.segments.iter().rev())
            .all(|(left, right)| left.looks_like_with(right, ctx))
    }
}

macro_rules! impl_looks_like_for_tokens {
    ( $( $t:ty ),* $(,)?) => {
        $(
            impl LooksLike for $t {
                fn looks_like_with(&self, _other: &Self, _ctx: &dyn Context) -> bool {
                    true
                }
            }
        )*
    };
}

impl_looks_like_for_tokens!(
    Token![for],
    Token![as],
    Token![unsafe],
    Token![:],
    Token![mut],
    Token![const],
    Token![::],
);

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_generics() {
        let ctx: syn::Generics = syn::parse_quote!(<T, anyA>);

        let self_ty_1: syn::Type = syn::parse_quote!(u8);
        let self_ty_2: syn::Type = syn::parse_quote!(T);

        assert_eq!(self_ty_2.looks_like_with(&self_ty_1, &ctx), true);
        assert_eq!(self_ty_1.looks_like_with(&self_ty_2, &ctx), true);
    }
}
