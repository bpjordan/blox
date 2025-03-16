use std::borrow::Cow;

use derive_more::Display;
use miette::SourceSpan;

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display("{kind}")]
pub struct Token<'de> {
    kind: TokenKind<'de>,
    span: SourceSpan,
}

impl<'de> Token<'de> {
    pub fn new(kind: TokenKind<'de>, span: impl Into<SourceSpan>) -> Self {
        Self {
            kind,
            span: span.into(),
        }
    }

    pub fn kind(&self) -> &TokenKind<'de> {
        &self.kind
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum TokenKind<'de> {
    // Single-character tokens.
    #[display("(")]
    LeftParen,
    #[display(")")]
    RightParen,
    #[display("{{")]
    LeftBrace,
    #[display("}}")]
    RightBrace,
    #[display(",")]
    Comma,
    #[display(".")]
    Dot,
    #[display("-")]
    Minus,
    #[display("+")]
    Plus,
    #[display(";")]
    Semicolon,
    #[display("/")]
    Slash,
    #[display("*")]
    Star,

    // One or two character tokens.
    #[display("!")]
    Bang,
    #[display("!=")]
    BangEqual,
    #[display("=")]
    Equal,
    #[display("==")]
    EqualEqual,
    #[display(">")]
    Greater,
    #[display(">=")]
    GreaterEqual,
    #[display("<")]
    Less,
    #[display("<=")]
    LessEqual,

    // Literals.
    #[display("{_0}")]
    Identifier(&'de str),
    #[display("\"{_0}\"")]
    String(Cow<'de, str>),
    #[display("{}", if let Some(f) = _1 { format!("{_0}.{f}") } else {format!("{_0}")} )]
    Number(u64, Option<u64>),

    #[display("")]
    Comment(&'de str),

    // Keywords.
    #[display("and")]
    And,
    #[display("class")]
    Class,
    #[display("else")]
    Else,
    #[display("false")]
    False,
    #[display("fun")]
    Fun,
    #[display("for")]
    For,
    #[display("if")]
    If,
    #[display("nil")]
    Nil,
    #[display("or")]
    Or,
    #[display("print")]
    Print,
    #[display("return")]
    Return,
    #[display("super")]
    Super,
    #[display("this")]
    This,
    #[display("true")]
    True,
    #[display("var")]
    Var,
    #[display("while")]
    While,
}
