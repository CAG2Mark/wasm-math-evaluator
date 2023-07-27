use std::collections::VecDeque;

use crate::ast::tokens::OtherFn;
use crate::ast::tokens::PrefixFn;
use crate::ast::tokens::Operator;
use crate::ast::tokens::Token;
use crate::ast::tokens::TokenPos;
use std::f64::consts;

type Position = usize;

const PREFIX_FUNCTIONS: &'static [(&'static str, PrefixFn)] = &[
    ("sin", PrefixFn::Sin),
    ("cos", PrefixFn::Cos),
    ("tan", PrefixFn::Tan),
    ("arcsin", PrefixFn::Arcsin),
    ("arccos", PrefixFn::Arccos),
    ("arctan", PrefixFn::Arctan),
    ("sinh", PrefixFn::Sinh),
    ("cosh", PrefixFn::Cosh),
    ("tanh", PrefixFn::Tanh),
    ("arsinh", PrefixFn::Arsinh),
    ("arcosh", PrefixFn::Arcosh),
    ("artanh", PrefixFn::Artanh),
    ("exp", PrefixFn::Exp),
    ("ln", PrefixFn::Ln),
    ("sqrt", PrefixFn::Sqrt),
    ("sgn", PrefixFn::Sgn),
    ("floor", PrefixFn::Floor),
    ("ceil", PrefixFn::Ceil),
];

const OTHER_FUNCTIONS: &'static [(&'static str, OtherFn)] = &[
    ("nCr", OtherFn::Ncr),
    ("nPr", OtherFn::Npr),
];

const OPS: &'static [(&'static str, Operator)] = &[
    ("+", Operator::Plus),
    ("-", Operator::Minus),
    ("*", Operator::Times),
    ("/", Operator::Div),
    ("%", Operator::Mod),
    ("!", Operator::Factorial),
    ("^", Operator::Pow),
];

const MATH_CONSTS: &'static [(&'static str, f64)] = &[
    ("pi", consts::PI),
    ("e", consts::E),
    ("phi", 1.618033988749894848205),
];

const WHITESPACE: &'static [char] = &[
    ' ', '\t', '\n'
];

fn try_lex_prefix_fn(input: &str) -> Option<(Token, usize)> {
    for (s, f) in PREFIX_FUNCTIONS {
        if input.starts_with(s) {
            return Some((Token::PrefixFunction(*f), s.len()));
        }
    }
    None
}

fn try_lex_math_const(input: &str) -> Option<(Token, usize)> {
    for (s, c) in MATH_CONSTS {
        if input.starts_with(s) {
            return Some((Token::Number(*c), s.len()));
        }
    }
    None
}

fn try_lex_other_fn(input: &str) -> Option<(Token, usize)> {
    for (s, f) in OTHER_FUNCTIONS {
        if input.starts_with(s) {
            return Some((Token::OtherFunction(*f), s.len()));
        }
    }
    None
}


fn try_lex_op(input: &str) -> Option<(Token, usize)> {
    for (s, o) in OPS {
        if input.starts_with(s) {
            return Some((Token::Op(*o), s.len()));
        }
    }
    None
}

fn try_lex_variable(input: &str) -> Option<(Token, usize)> {
    let ch = input.chars().next();

    match ch {
        Some(ch) if ch.is_alphabetic() => Some((Token::Variable(ch), 1)),
        _ => None,
    }
}

fn try_lex_imaginary(input: &str) -> Option<(Token, usize)> {
    let ch = input.chars().next();

    match ch {
        Some(ch) if ch == 'i' => Some((Token::ImaginaryConst, 1)),
        _ => None,
    }
}

fn try_lex_number(input: &str) -> Option<(Token, usize)> {
    let mut it = input.chars();
    let mut seen_dot = false;

    let mut cur = it.next();

    let mut len = 0;

    // while the next char exists, and it is a digit, or it is a decimal point/comma if we have not seen one already
    while cur.is_some()
        && (cur.unwrap().is_numeric()
            || (!seen_dot && (cur.unwrap() == '.' || cur.unwrap() == ',')))
    {
        len += 1;

        // there must be a digit after a dot
        if cur.unwrap() == '.' || cur.unwrap() == ',' {
            seen_dot = true;
            cur = it.next();
            len += 1;
            
            if cur.is_none() || !cur.unwrap().is_numeric() {
                len -= 2;
                break;
            }
        }
        
        cur = it.next();
    }

    if len == 0 {
        return None
    }

    let sliced = &input[0..len];

    let num = sliced.parse().unwrap();

    Some((Token::Number(num), len))
}

fn try_lex_brace(input: &str) -> Option<(Token, usize)> {
    let ch = input.chars().next();

    match ch {
        Some('(') => Some((Token::OpenBrace, 1)),
        Some(')') => Some((Token::CloseBrace, 1)),
        _ => None,
    }
}

fn try_lex_whitespace(input: &str) -> Option<(Token, usize)> {
    let mut it = input.chars();
    let mut cur = it.next();

    let mut len = 0;

    // while the next char exists, and it is a digit, or it is a decimal point/comma if we have not seen one already
    while cur.is_some()
        && (WHITESPACE.contains(&cur.unwrap()))
    {
        len += 1;
        cur = it.next();
    }

    if len == 0 {
        return None
    }

    Some((Token::Whitespace, len))
}

type Lexer = fn(&str) -> Option<(Token, usize)>;

const LEXERS: &'static [Lexer] = &[
    try_lex_whitespace,
    try_lex_prefix_fn,
    try_lex_other_fn,
    try_lex_math_const,
    try_lex_number,
    try_lex_op,
    try_lex_imaginary,
    try_lex_variable,
    try_lex_brace,
];

pub fn lex(input: &str) -> Result<VecDeque<TokenPos>, Position> {
    let mut pos: usize = 0;
    let mut ret = VecDeque::new();

    let mut progress = true;

    while progress {
        progress = false;

        let slice = &input[pos..];

        for f in LEXERS {
            match f(slice) {
                Some((tk, len)) => {
                    if !matches!(tk, Token::Whitespace) {
                        ret.push_back(TokenPos { tk, pos: (pos, pos + len) });
                    }
                    pos += len;
                    progress = true;
                    break;
                },
                None => continue,
            } 
        }
    }

    if pos == input.len() {
        Ok(ret)
    } else {
        Err(pos)
    }
}
