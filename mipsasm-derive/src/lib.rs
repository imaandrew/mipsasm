extern crate proc_macro;

use crate::proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Op)]
pub fn op_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let st_name = input.ident;
    TokenStream::from(quote! {
        #[automatically_derived]
        impl crate::ast::Op for #st_name {
            fn as_num(&self) -> u8 {
                *self as u8
            }
        }
    })
}

#[proc_macro_derive(Reg)]
pub fn reg_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let st_name = input.ident;
    TokenStream::from(quote! {
        #[automatically_derived]
        impl crate::ast::Reg for #st_name {
            fn as_num(&self) -> u8 {
                *self as u8
            }
        }
    })
}
