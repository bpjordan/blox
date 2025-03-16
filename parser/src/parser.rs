use std::iter::Peekable;

use thiserror::Error;

use crate::{
    ast::{BinaryExpr, Expression, Grouping, UnaryExpr},
    lexer::{self, Lexer},
    tokens::{Token, TokenKind},
};

pub type Result<'de, T, E = Error<'de>> = std::result::Result<T, E>;

#[derive(Debug, Clone, Error)]
#[error(transparent)]
pub struct Error<'de> {
    kind: ErrorKind<'de>,
}

impl<'de> Error<'de> {
    pub fn kind(&self) -> &ErrorKind<'de> {
        &self.kind
    }
}

#[derive(Debug, Clone, Error)]
pub enum ErrorKind<'de> {
    #[error(transparent)]
    Lex(lexer::Error<'de>),
    #[error("unexpected token (expected {1}, got {0})")]
    UnexpectedToken(TokenKind<'de>, String),
    #[error("unexpected end of file")]
    UnexpectedEof,
}

pub struct Parser<'de, L>
where
    L: Iterator<Item = lexer::Result<'de>>,
{
    lexer: Peekable<L>,
}

impl<'de> Parser<'de, Lexer<'de>> {
    pub fn for_str(src: &'de str) -> Self {
        Parser {
            lexer: Lexer::for_str(src).peekable(),
        }
    }
}

impl<'de, L> Parser<'de, L>
where
    L: Iterator<Item = lexer::Result<'de>>,
{
    pub fn for_tokens(
        tokens: &[Token<'de>],
    ) -> Parser<'de, impl Iterator<Item = lexer::Result<'de>>> {
        Parser {
            lexer: tokens.iter().cloned().map(Result::Ok).peekable(),
        }
    }
}

impl<'de, L> Parser<'de, L>
where
    L: Iterator<Item = lexer::Result<'de>>,
{
    pub fn parse(&mut self) -> Result<'de, Expression<'de>> {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Result<'de, Expression<'de>> {
        self.parse_equality()
    }

    /// General-purpose helper to parse grammars of the form `child (op child)*`
    fn parse_binary_expr(
        &mut self,
        parse_child: impl Fn(&mut Self) -> Result<'de, Expression<'de>>,
        match_op: impl Fn(&Token) -> bool,
    ) -> Result<'de, Expression<'de>> {
        let mut expr = parse_child(self)?;

        while let Some(t) = self.peek() {
            if !match_op(&t?) {
                break;
            }

            let op = self.next_token()?;
            let rhs = parse_child(self)?;

            expr = BinaryExpr::new(expr, op, rhs).into();
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<'de, Expression<'de>> {
        self.parse_binary_expr(Self::parse_comparison, |t| {
            matches!(t.kind(), TokenKind::EqualEqual | TokenKind::BangEqual)
        })
    }

    fn parse_comparison(&mut self) -> Result<'de, Expression<'de>> {
        self.parse_binary_expr(Self::parse_term, |t| {
            matches!(
                t.kind(),
                TokenKind::Greater
                    | TokenKind::GreaterEqual
                    | TokenKind::Less
                    | TokenKind::LessEqual
            )
        })
    }

    fn parse_term(&mut self) -> Result<'de, Expression<'de>> {
        self.parse_binary_expr(Self::parse_factor, |t| {
            matches!(t.kind(), TokenKind::Minus | TokenKind::Plus)
        })
    }

    fn parse_factor(&mut self) -> Result<'de, Expression<'de>> {
        self.parse_binary_expr(Self::parse_unary, |t| {
            matches!(t.kind(), TokenKind::Slash | TokenKind::Star)
        })
    }

    fn parse_unary(&mut self) -> Result<'de, Expression<'de>> {
        let t = self.peek().transpose()?;

        if t.is_some_and(|i| matches!(i.kind(), TokenKind::Bang | TokenKind::Minus)) {
            let op = self.next_token()?;
            let expr = self.parse_unary()?;
            Ok(UnaryExpr::new(op, expr).into())
        } else {
            Ok(self.parse_primary()?)
        }
    }

    fn parse_primary(&mut self) -> Result<'de, Expression<'de>> {
        let t = self.next_token()?;
        match t.kind() {
            TokenKind::False
            | TokenKind::True
            | TokenKind::Nil
            | TokenKind::Number(_, _)
            | TokenKind::String(_) => Ok(Expression::Literal(t)),

            TokenKind::LeftParen => {
                let expr = self.parse_expression()?;

                self.expect(TokenKind::RightParen)?;

                Ok(Grouping::new(expr).into())
            }

            t => Err(self.error(ErrorKind::UnexpectedToken(
                t.clone(),
                "expression".to_string(),
            ))),
        }
    }
}

impl<'de, L> Parser<'de, L>
where
    L: Iterator<Item = lexer::Result<'de>>,
{
    fn error(&self, kind: ErrorKind<'de>) -> Error<'de> {
        Error { kind }
    }

    fn peek(&mut self) -> Option<Result<'de, Token<'de>>> {
        self.lexer
            .peek()
            .cloned()
            .map(|r| r.map_err(|e| self.error(ErrorKind::Lex(e.clone()))))
    }

    fn next_token(&mut self) -> Result<'de, Token<'de>> {
        self.lexer
            .next()
            .transpose()
            .map_err(|e| self.error(ErrorKind::Lex(e.clone())))
            .and_then(|o| o.ok_or_else(|| self.error(ErrorKind::UnexpectedEof)))
    }

    #[must_use]
    fn expect(&mut self, kind: TokenKind<'static>) -> Result<'de, Token<'de>> {
        let t = self.next_token()?;

        if t.kind() == &kind {
            Ok(t)
        } else {
            Err(self.error(ErrorKind::UnexpectedToken(
                t.kind().clone(),
                kind.to_string(),
            )))
        }
    }

    #[allow(dead_code)]
    fn synchronize(&mut self) {
        while let Some(r) = self.lexer.next() {
            let Ok(t) = r else { continue };

            if t.kind() == &TokenKind::Semicolon {
                return;
            }

            let Some(Ok(peek)) = self.peek() else {
                continue;
            };

            if matches!(
                peek.kind(),
                TokenKind::Class
                    | TokenKind::Fun
                    | TokenKind::Var
                    | TokenKind::For
                    | TokenKind::If
                    | TokenKind::While
                    | TokenKind::Print
                    | TokenKind::Return
            ) {
                return;
            }
        }
    }
}
