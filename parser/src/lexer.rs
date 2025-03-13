use std::{borrow::Cow, str::CharIndices};

use miette::{Diagnostic, SourceOffset, SourceSpan};
use phf::{Map, phf_map};
use thiserror::Error;

const KEYWORDS: Map<&'static str, TokenKind<'static>> = phf_map!(
    "and" => TokenKind::And,
    "class" => TokenKind::Class,
    "else" => TokenKind::Else,
    "false" => TokenKind::False,
    "fun" => TokenKind::Fun,
    "for" => TokenKind::For,
    "if" => TokenKind::If,
    "nil" => TokenKind::Nil,
    "or" => TokenKind::Or,
    "print" => TokenKind::Print,
    "return" => TokenKind::Return,
    "super" => TokenKind::Super,
    "this" => TokenKind::This,
    "true" => TokenKind::True,
    "var" => TokenKind::Var,
    "while" => TokenKind::While,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'de> {
    kind: TokenKind<'de>,
    span: SourceSpan,
}

impl<'de> Token<'de> {
    fn new(kind: TokenKind<'de>, span: impl Into<SourceSpan>) -> Self {
        Self {
            kind,
            span: span.into(),
        }
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

#[derive(Debug, Error, Diagnostic, PartialEq, Eq)]
#[error("Failed to parse token: {kind}")]
pub struct LexError<'de> {
    kind: LexErrorKind,
    #[source_code]
    src: &'de str,
    #[label("here")]
    span: SourceSpan,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum LexErrorKind {
    #[error("Unexpected character {0}")]
    UnexpectedChar(char),

    #[error("Unterminated String")]
    UnterminatedString,

    #[error("Failed to parse number")]
    NumberParse,
}

pub struct Lexer<'de> {
    src: &'de str,
    chars: CharIndices<'de>,
}

impl<'de> Lexer<'de> {
    pub fn for_str(src: &'de str) -> Self {
        Lexer {
            src,
            chars: src.char_indices(),
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, LexError<'de>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let rest = self.chars.as_str();
            let (idx, c) = self.chars.next()?;

            break match c {
                w if w.is_whitespace() => continue,
                '(' => Some(Ok(Token::new(TokenKind::LeftParen, idx))),
                ')' => Some(Ok(Token::new(TokenKind::RightParen, idx))),
                '{' => Some(Ok(Token::new(TokenKind::LeftBrace, idx))),
                '}' => Some(Ok(Token::new(TokenKind::RightBrace, idx))),
                ',' => Some(Ok(Token::new(TokenKind::Comma, idx))),
                '.' => Some(Ok(Token::new(TokenKind::Dot, idx))),
                '-' => Some(Ok(Token::new(TokenKind::Minus, idx))),
                '+' => Some(Ok(Token::new(TokenKind::Plus, idx))),
                ';' => Some(Ok(Token::new(TokenKind::Semicolon, idx))),
                '*' => Some(Ok(Token::new(TokenKind::Star, idx))),
                '!' => Some(self.parse_double('=', TokenKind::Bang, TokenKind::BangEqual, idx)),
                '=' => Some(self.parse_double('=', TokenKind::Equal, TokenKind::EqualEqual, idx)),
                '<' => Some(self.parse_double('=', TokenKind::Less, TokenKind::LessEqual, idx)),
                '>' => {
                    Some(self.parse_double('=', TokenKind::Greater, TokenKind::GreaterEqual, idx))
                }
                '/' => Some(self.parse_comment(idx)),
                '"' => Some(self.parse_string(idx)),
                n if n.is_numeric() => Some(self.parse_num(idx, rest)),
                a if a.is_alphabetic() => Some(self.parse_word(idx, rest)),
                '_' => Some(self.parse_word(idx, rest)),
                u => Some(Err(self.make_error(LexErrorKind::UnexpectedChar(u), idx))),
            };
        }
    }
}

impl<'de> Lexer<'de> {
    fn peek_next_char(&self) -> Option<char> {
        self.chars.clone().next().map(|(_, c)| c)
    }

    fn peek_2nd_char(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().map(|(_, c)| c)
    }

    fn make_error(&self, kind: LexErrorKind, start_idx: usize) -> LexError<'de> {
        LexError {
            kind,
            src: &self.src,
            span: SourceSpan::new(
                SourceOffset::from(start_idx),
                self.chars.offset() - start_idx,
            ),
        }
    }

    fn parse_double(
        &mut self,
        next_char: char,
        single_kind: TokenKind<'de>,
        double_kind: TokenKind<'de>,
        idx: usize,
    ) -> <Self as Iterator>::Item {
        if self.peek_next_char() == Some(next_char) {
            self.chars.next();
            Ok(Token::new(double_kind, idx..idx + 1))
        } else {
            Ok(Token::new(single_kind, idx))
        }
    }

    fn parse_comment(&mut self, idx: usize) -> <Self as Iterator>::Item {
        if self.peek_next_char() != Some('/') {
            return Ok(Token::new(TokenKind::Slash, idx));
        }

        self.chars.next();

        let rest = self.chars.as_str();

        let end_idx = loop {
            match self.chars.next() {
                None => break self.chars.offset() - 1,
                Some((i, '\n')) => break i - 1,
                Some(_) => continue,
            }
        };

        let comment_str = &rest[..end_idx - (idx + 1)];

        Ok(Token::new(
            TokenKind::Comment(comment_str.trim_start()),
            idx..=end_idx,
        ))
    }

    // TODO: Escapes?? Interpolation????
    fn parse_string(&mut self, idx: usize) -> <Self as Iterator>::Item {
        let rest = self.chars.as_str();

        let end_idx = loop {
            match self.chars.next() {
                None | Some((_, '\n')) => {
                    return Err(self.make_error(LexErrorKind::UnterminatedString, idx));
                }
                Some((i, '"')) => break i,
                Some(_) => continue,
            }
        };

        let string = &rest[..end_idx - (idx + 1)];

        Ok(Token::new(
            TokenKind::String(Cow::Borrowed(string)),
            idx..=end_idx,
        ))
    }

    fn parse_num(&mut self, idx: usize, rest: &'de str) -> <Self as Iterator>::Item {
        let (whole_end, is_float) = loop {
            match self.peek_next_char() {
                Some(d) if d.is_numeric() => {
                    self.chars.next();
                    continue;
                }
                Some('.') if self.peek_2nd_char().is_some_and(char::is_numeric) => {
                    self.chars.next();
                    break (self.chars.offset() - 1, true);
                }
                _ => break (self.chars.offset(), false),
            }
        };

        let Ok(whole_part) = rest[..whole_end - idx].parse() else {
            return Err(self.make_error(LexErrorKind::NumberParse, idx));
        };

        let deci_end = if is_float {
            loop {
                match self.peek_next_char() {
                    Some(d) if d.is_numeric() => {
                        self.chars.next();
                        continue;
                    }
                    _ => (),
                }

                break Some(self.chars.offset());
            }
        } else {
            None
        };

        let Ok(deci_part) = deci_end
            .map(|i| rest[whole_end - idx + 1..i - idx].parse())
            .transpose()
        else {
            return Err(self.make_error(LexErrorKind::NumberParse, idx));
        };

        Ok(Token::new(
            TokenKind::Number(whole_part, deci_part),
            idx..deci_end.unwrap_or(whole_end),
        ))
    }

    fn parse_word(&mut self, idx: usize, rest: &'de str) -> <Self as Iterator>::Item {
        let end_idx = loop {
            match self.peek_next_char() {
                Some(w) if w.is_alphanumeric() => {
                    self.chars.next();
                    continue;
                }
                Some('_') => {
                    self.chars.next();
                    continue;
                }
                _ => {
                    break self.chars.offset();
                }
            }
        };

        let ident = &rest[..end_idx - idx];

        let kind = KEYWORDS
            .get(ident)
            .cloned()
            .unwrap_or_else(|| TokenKind::Identifier(ident));

        Ok(Token::new(kind, idx..end_idx))
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use super::{Lexer, Token, TokenKind};

    fn assert_tokens(src: &str, expected: impl IntoIterator<Item = Token<'static>>) {
        let mut lexer = Lexer::for_str(src);

        for expected_token in expected {
            assert_eq!(lexer.next().unwrap().unwrap(), expected_token);
        }

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn parse_punctuation() {
        assert_tokens(
            "(){};,+-*!===<=>=!=<>/.",
            [
                Token::new(TokenKind::LeftParen, 0),
                Token::new(TokenKind::RightParen, 1),
                Token::new(TokenKind::LeftBrace, 2),
                Token::new(TokenKind::RightBrace, 3),
                Token::new(TokenKind::Semicolon, 4),
                Token::new(TokenKind::Comma, 5),
                Token::new(TokenKind::Plus, 6),
                Token::new(TokenKind::Minus, 7),
                Token::new(TokenKind::Star, 8),
                Token::new(TokenKind::BangEqual, 9..10),
                Token::new(TokenKind::EqualEqual, 11..12),
                Token::new(TokenKind::LessEqual, 13..14),
                Token::new(TokenKind::GreaterEqual, 15..16),
                Token::new(TokenKind::BangEqual, 17..18),
                Token::new(TokenKind::Less, 19),
                Token::new(TokenKind::Greater, 20),
                Token::new(TokenKind::Slash, 21),
                Token::new(TokenKind::Dot, 22),
            ],
        );
    }

    #[test]
    fn parse_comment() {
        assert_tokens(
            "// foo bar\n;\n//baz",
            [
                Token::new(TokenKind::Comment("foo bar"), 0..10),
                Token::new(TokenKind::Semicolon, 11),
                Token::new(TokenKind::Comment("baz"), 13..18),
            ],
        )
    }

    #[test]
    fn parse_string() {
        assert_tokens(
            r#""string""#,
            [Token::new(TokenKind::String(Cow::Borrowed("string")), 0..8)],
        );
    }

    #[test]
    fn parse_nums() {
        assert_tokens(
            "12 1.2 1.",
            [
                Token::new(TokenKind::Number(12, None), 0..2),
                Token::new(TokenKind::Number(1, Some(2)), 3..6),
                Token::new(TokenKind::Number(1, None), 7..8),
                Token::new(TokenKind::Dot, 8),
            ],
        );
    }

    #[test]
    fn parse_keywords() {
        assert_tokens(
            "and class else false for fun if nil or return super this true var while",
            [
                Token::new(TokenKind::And, 0..3),
                Token::new(TokenKind::Class, 4..9),
                Token::new(TokenKind::Else, 10..14),
                Token::new(TokenKind::False, 15..20),
                Token::new(TokenKind::For, 21..24),
                Token::new(TokenKind::Fun, 25..28),
                Token::new(TokenKind::If, 29..31),
                Token::new(TokenKind::Nil, 32..35),
                Token::new(TokenKind::Or, 36..38),
                Token::new(TokenKind::Return, 39..45),
                Token::new(TokenKind::Super, 46..51),
                Token::new(TokenKind::This, 52..56),
                Token::new(TokenKind::True, 57..61),
                Token::new(TokenKind::Var, 62..65),
                Token::new(TokenKind::While, 66..71),
            ],
        );
    }

    #[test]
    fn parse_idents() {
        assert_tokens(
            "foo bar_ _baz",
            [
                Token::new(TokenKind::Identifier("foo"), 0..3),
                Token::new(TokenKind::Identifier("bar_"), 4..8),
                Token::new(TokenKind::Identifier("_baz"), 9..13),
            ],
        );
    }
}
