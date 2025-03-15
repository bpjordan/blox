use derive_more::{Display, From};

use crate::tokens::Token;

#[derive(Debug, Display, From)]
pub enum Expression<'de> {
    #[from(ignore)]
    Literal(Token<'de>),
    #[from(UnaryExpr<'de>)]
    Unary(Box<UnaryExpr<'de>>),
    #[from(BinaryExpr<'de>)]
    Binary(Box<BinaryExpr<'de>>),
    #[from(Grouping<'de>)]
    Grouping(Box<Grouping<'de>>),
}

#[derive(Debug, Display)]
#[display("(group {expr})")]
pub struct Grouping<'de> {
    expr: Expression<'de>,
}

impl<'de> Grouping<'de> {
    pub fn new(expr: impl Into<Expression<'de>>) -> Self {
        Self { expr: expr.into() }
    }
}

#[derive(Debug, Display)]
#[display("({op} {expr})")]
pub struct UnaryExpr<'de> {
    op: Token<'de>,
    expr: Expression<'de>,
}

impl<'de> UnaryExpr<'de> {
    pub fn new(op: Token<'de>, expr: impl Into<Expression<'de>>) -> Self {
        Self {
            op,
            expr: expr.into(),
        }
    }
}

#[derive(Debug, Display)]
#[display("({op} {lhs} {rhs})")]
pub struct BinaryExpr<'de> {
    lhs: Expression<'de>,
    op: Token<'de>,
    rhs: Expression<'de>,
}

impl<'de> BinaryExpr<'de> {
    pub fn new(
        lhs: impl Into<Expression<'de>>,
        op: Token<'de>,
        rhs: impl Into<Expression<'de>>,
    ) -> Self {
        Self {
            lhs: lhs.into(),
            op,
            rhs: rhs.into(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::tokens::{Token, TokenKind};

    use super::{BinaryExpr, Expression, Grouping, UnaryExpr};

    #[test]
    fn display() {
        let ast = Expression::from(BinaryExpr::new(
            UnaryExpr::new(
                Token::new(TokenKind::Minus, 0),
                Expression::Literal(Token::new(TokenKind::Number(123, None), 0)),
            ),
            Token::new(TokenKind::Star, 0),
            Grouping::new(Expression::Literal(Token::new(
                TokenKind::Number(45, Some(67)),
                0,
            ))),
        ));

        assert_eq!(ast.to_string(), "(* (- 123) (group 45.67))")
    }
}
