use std::{marker::PhantomData, str::CharIndices};

pub struct Token<'de> {
    kind: TokenKind<'de>,
    span: (u8, Option<u8>),
}

pub enum TokenKind<'de> {
    Todo(PhantomData<&'de str>),
}

pub struct Lexer<'de> {
    chars: CharIndices<'de>,
}

impl<'de> Lexer<'de> {
    pub fn for_str(src: &'de str) -> Self {
        Lexer {
            chars: src.char_indices(),
        }
    }
}
