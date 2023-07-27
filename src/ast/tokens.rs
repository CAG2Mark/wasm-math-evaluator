use std::fmt;

#[derive(Copy, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Factorial,
    Pow
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
    Ceil
}

#[derive(Copy, Clone)]
pub enum OtherFn {
    Ncr,
    Npr
}

#[derive(Copy, Clone)]
pub enum Token {
    Number(f64),
    Op(Operator),
    Variable(char),
    PrefixFunction(PrefixFn),
    OtherFunction(OtherFn),
    OpenBrace,
    CloseBrace,
    ImaginaryConst,
    Whitespace
}

pub(crate) type Position = (usize, usize);

#[derive(Copy, Clone)]
pub struct TokenPos {
    pub tk: Token,
    pub pos: Position
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
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for OtherFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            OtherFn::Ncr => "nCr",
            OtherFn::Npr => "nPr",
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Token::Number(n) => n.to_string(),
            Token::Op(op) => op.to_string(),
            Token::Variable(ch) => ch.to_string(),
            Token::PrefixFunction(f) => f.to_string(),
            Token::OpenBrace => "(".to_string(),
            Token::CloseBrace => ")".to_string(),
            Token::Whitespace => "<whitespace>".to_string(),
            Token::ImaginaryConst => "i".to_string(),
            Token::OtherFunction(f) => f.to_string(),
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for TokenPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.tk)
    }
}