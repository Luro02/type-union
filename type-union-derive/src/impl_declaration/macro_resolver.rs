use syn::fold::Fold;

use crate::utils::is_macro;

fn expand(
    mut mac: syn::Macro,
    f: impl FnOnce(proc_macro::TokenStream) -> proc_macro::TokenStream,
) -> syn::Macro {
    mac.path = syn::parse_quote!(__identity);
    mac.tokens = f(mac.tokens.into()).into();

    mac
}

#[derive(Debug, Clone, Default)]
pub struct MacroResolver {}

impl MacroResolver {
    /// Returns the macro that returns its input without changing it.
    ///
    /// This is used to keep the `Fold` api simple. The caller is responsible
    /// to place this macro declaration in the right place in the output.
    #[must_use]
    pub fn identity_macro(&self) -> syn::ItemMacro {
        syn::parse_quote! {
            macro_rules! __identity {
                ( $($t:tt)* ) => { $( $t )* };
            }
        }
    }
}

impl Fold for MacroResolver {
    fn fold_macro(&mut self, mut mac: syn::Macro) -> syn::Macro {
        if is_macro(&mac, "type_union") {
            mac = expand(mac, crate::type_union);
        } else if is_macro(&mac, "define_type_union") {
            mac = expand(mac, crate::define_type_union);
        } else if is_macro(&mac, "match_type_union") {
            mac = expand(mac, crate::match_type_union);
        }

        syn::fold::fold_macro(self, mac)
    }
}
