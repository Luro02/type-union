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
    fn looks_like(&self, other: &T) -> bool;
}

impl LooksLike for syn::PathSegment {
    fn looks_like(&self, other: &Self) -> bool {
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
            ) => {
                let mut look_equal = true;
                for (left_arg, right_arg) in left.args.iter().zip(right.args.iter()) {
                    match (left_arg, right_arg) {
                        (syn::GenericArgument::Type(lty), syn::GenericArgument::Type(rty)) => {
                            let is_left_macro = matches!(lty, syn::Type::Macro(_));
                            let is_right_macro = matches!(rty, syn::Type::Macro(_));
                            // if one type is a macro, the other must also be a macro
                            look_equal = is_left_macro && is_right_macro
                                || !is_left_macro && !is_right_macro;
                        }
                        _ => {}
                    }
                }

                look_equal
            }
            _ => false,
        }
    }
}

impl LooksLike for syn::Path {
    fn looks_like(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        // iter in reverse order, so qualified and unqualified paths are considered equal
        // for example `marker::Copy` and `::core::marker::Copy` would be considered equal
        self.segments
            .iter()
            .rev()
            .zip(other.segments.iter().rev())
            .all(|(left, right)| left.looks_like(right))
    }
}
