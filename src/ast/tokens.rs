use num_bigfloat::BigFloat;
use std::fmt;

#[derive(Copy, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Factorial,
    Pow,
}

#[derive(Copy, Clone)]
pub enum PrefixFn {
    Sin,
    Cos,
    Tan,
    Arcsin,
    Arccos,
    Arctan,
    Sinh,
    Cosh,
    Tanh,
    Arsinh,
    Arcosh,
    Artanh,
    Exp,
    Ln,
    Sqrt,
    Sgn,
    Floor,
    Ceil,
    Abs,
}

#[derive(Copy, Clone)]
pub enum MathConst {
    PI,
    E,
    PHI,
}

#[derive(Copy, Clone)]
pub enum OtherFn {
    Ncr,
    Npr,
    Atan2,
    Min,
    Max
}

#[derive(Copy, Clone)]
pub enum Token {
    Number(BigFloat, usize), // number of decimal places
    Const(MathConst),
    Op(Operator),
    Variable(char),
    PrefixFunction(PrefixFn),
    OtherFunction(OtherFn),
    OpenBrace,
    CloseBrace,
    Semicolon,
    Comma,
    // ImaginaryConst,
    Whitespace,
    Let,
    Equals
}

pub(crate) type Position = (usize, usize);

#[derive(Copy, Clone)]
pub struct TokenPos {
    pub tk: Token,
    pub pos: Position,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Operator::Plus => '+',
            Operator::Minus => '-',
            Operator::Times => '*',
            Operator::Div => '/',
            Operator::Mod => '%',
            Operator::Factorial => '!',
            Operator::Pow => '^',
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for PrefixFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            PrefixFn::Sin => "sin",
            PrefixFn::Cos => "cos",
            PrefixFn::Tan => "tan",
            PrefixFn::Arcsin => "arcsin",
            PrefixFn::Arccos => "arccos",
            PrefixFn::Arctan => "arctan",
            PrefixFn::Sinh => "sinh",
            PrefixFn::Cosh => "cosh",
            PrefixFn::Tanh => "tanh",
            PrefixFn::Arsinh => "arsinh",
            PrefixFn::Arcosh => "arcosh",
            PrefixFn::Artanh => "artanh",
            PrefixFn::Exp => "exp",
            PrefixFn::Ln => "ln",
            PrefixFn::Sqrt => "sqrt",
            PrefixFn::Sgn => "sgn",
            PrefixFn::Floor => "floor",
            PrefixFn::Ceil => "ceil",
            PrefixFn::Abs => "abs",
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for MathConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            MathConst::PI => "pi",
            MathConst::E => "e",
            MathConst::PHI => "phi",
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for OtherFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            OtherFn::Ncr => "nCr",
            OtherFn::Npr => "nPr",
            OtherFn::Atan2 => "atan2",
            OtherFn::Min => "min",
            OtherFn::Max => "max",
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Token::Number(n, d) => format!("{:.*}", d, n),
            Token::Op(op) => op.to_string(),
            Token::Variable(ch) => ch.to_string(),
            Token::PrefixFunction(f) => f.to_string(),
            Token::OpenBrace => "(".to_string(),
            Token::CloseBrace => ")".to_string(),
            Token::Whitespace => "<whitespace>".to_string(),
            // Token::ImaginaryConst => "i".to_string(),
            Token::OtherFunction(f) => f.to_string(),
            Token::Const(c) => c.to_string(),
            Token::Comma => ",".to_string(),
            Token::Let => "let".to_string(),
            Token::Equals => "=".to_string(),
            Token::Semicolon => ";".to_string(),
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for TokenPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.tk)
    }
}
