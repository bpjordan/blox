use std::{borrow::Cow, fmt::Display};

use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'de> {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(&'de str),
    String(Cow<'de, str>),
    Number(u64, Option<u64>),

    Comment(&'de str),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Identifier(ident) => write!(f, "{ident}"),
            TokenKind::String(cow) => write!(f, "\"{cow}\""),
            TokenKind::Number(whole, part) => {
                if let Some(frac) = part {
                    write!(f, "{whole}.{frac}")
                } else {
                    write!(f, "{whole}")
                }
            }
            TokenKind::Comment(_) => Ok(()),
            TokenKind::And => write!(f, "and"),
            TokenKind::Class => write!(f, "class"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Fun => write!(f, "fun"),
            TokenKind::For => write!(f, "for"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Print => write!(f, "print"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Super => write!(f, "super"),
            TokenKind::This => write!(f, "this"),
            TokenKind::True => write!(f, "true"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::While => write!(f, "while"),
        }
    }
}
