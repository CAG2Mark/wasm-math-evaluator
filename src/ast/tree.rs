use std::fmt;

use num_bigfloat::BigFloat;

use crate::ast::tokens::Operator;
use crate::ast::tokens::OtherFn;
use crate::ast::tokens::Position;
use crate::ast::tokens::PrefixFn;
use crate::ast::tokens::MathConst;

pub enum Expr {
    InfixOp(Operator, Box<ExprPos>, Box<ExprPos>),
    PrefixFn(PrefixFn, Box<ExprPos>),
    PrefixOp(Operator, Box<ExprPos>),
    PostfixOp(Operator, Box<ExprPos>),
    Variable(char),
    Nested(Box<ExprPos>),
    Const(MathConst),
    OtherFunction(OtherFn),
    Number(BigFloat, usize),
    // ImaginaryConst
}

pub struct ExprPos {
    pub expr: Expr,
    pub pos: Position,
}

impl fmt::Display for ExprPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::InfixOp(op, lhs, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::PrefixFn(func, val) => write!(f, "({} {})", func, val),
            Expr::PrefixOp(op, val) => write!(f, "({}{})", op, val),
            Expr::PostfixOp(op, val) => write!(f, "({}{})", val, op),
            Expr::Variable(c) => write!(f, "{}", c),
            Expr::OtherFunction(_) => todo!(),
            // Expr::ImaginaryConst => write!(f, "i"),
            Expr::Number(n, d) => write!(f, "{:.*}", d, n),
            Expr::Const(c) => write!(f, "{}", c),
            Expr::Nested(e) => write!(f, "({})", e),
        }
    }
}