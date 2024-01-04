use std::collections::VecDeque;

use crate::ast::tokens::MathConst;
use crate::ast::tokens::Operator;
use crate::ast::tokens::OtherFn;
use crate::ast::tokens::PrefixFn;
use crate::ast::tokens::Token;
use crate::ast::tokens::TokenPos;

use num_bigfloat::BigFloat;

type Position = usize;

const PREFIX_FUNCTIONS: &'static [(&'static str, PrefixFn)] = &[
    ("arcsin", PrefixFn::Arcsin),
    ("arccos", PrefixFn::Arccos),
    ("arctan", PrefixFn::Arctan),
    ("sinh", PrefixFn::Sinh),
    ("cosh", PrefixFn::Cosh),
    ("tanh", PrefixFn::Tanh),
    ("sin", PrefixFn::Sin),
    ("cos", PrefixFn::Cos),
    ("tan", PrefixFn::Tan),
    ("arsinh", PrefixFn::Arsinh),
    ("arcosh", PrefixFn::Arcosh),
    ("artanh", PrefixFn::Artanh),
    ("exp", PrefixFn::Exp),
    ("ln", PrefixFn::Ln),
    ("sqrt", PrefixFn::Sqrt),
    ("sgn", PrefixFn::Sgn),
    ("floor", PrefixFn::Floor),
    ("ceil", PrefixFn::Ceil),
    ("abs", PrefixFn::Abs),
];

const OTHER_FUNCTIONS: &'static [(&'static str, OtherFn)] = &[
    ("nCr", OtherFn::Ncr), 
    ("nPr", OtherFn::Npr),
    ("atan2", OtherFn::Atan2),
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

const MATH_CONSTS: &'static [(&'static str, MathConst)] = &[
    ("pi", MathConst::PI),
    ("e", MathConst::E),
    ("phi", MathConst::PHI),
];

const WHITESPACE: &'static [char] = &[' ', '\t', '\n'];

fn try_lex_let(input: &str) -> Option<(Token, usize)> {
    if input.starts_with("let") {
        return Some((Token::Let, 3));
    }
    None
}

fn try_lex_equal(input: &str) -> Option<(Token, usize)> {
    if input.starts_with("=") {
        return Some((Token::Equals, 1));
    }
    None
}

fn try_lex_semicolon(input: &str) -> Option<(Token, usize)> {
    if input.starts_with(";") {
        return Some((Token::Semicolon, 1));
    }
    None
}

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
            return Some((Token::Const(*c), s.len()));
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

/*
fn try_lex_imaginary(input: &str) -> Option<(Token, usize)> {
    let ch = input.chars().next();

    match ch {
        // i was bullied by an electrical engineer so i have to parse `j` as well
        Some(ch) if ch == 'i' || ch == 'j' => Some((Token::ImaginaryConst, 1)),
        _ => None,
    }
}
*/

fn is_decimal_point(ch: char) -> bool {
    return ch == '.';
}

fn try_lex_number(input: &str) -> Option<(Token, usize)> {
    let mut it = input.chars();
    let mut seen_dot = false;

    let mut cur = it.next();

    let mut len = 0;

    let mut decimal_len = 0;

    // while the next char exists, and it is a digit, or it is a decimal point/comma if we have not seen one already
    while let Some(c) = cur {
        if !c.is_numeric() && (seen_dot || !is_decimal_point(c)) {
            break;
        }

        len += 1;
        if seen_dot {
            decimal_len += 1;
        }

        // there must be a digit after a dot
        if is_decimal_point(c) {
            seen_dot = true;
            cur = it.next();
            len += 1;
            decimal_len += 1;

            match cur {
                Some(c) if c.is_numeric() => {}
                _ => {
                    len -= 2;
                    break;
                }
            }
        }

        cur = it.next();
    }

    if len == 0 {
        return None;
    }

    let sliced = &input[0..len].replace(",", ".");

    // library is slightly buggy and parses "01" as "1e39"
    // so we need to trim away the zeros at the start
    
    let mut cur: usize = len;
    let chars = sliced.chars();
    let mut i = 0;
    for ch in chars {
        if ch != '0' {
            cur = i;
            break;
        }
        i += 1;
    }

    if cur == len {
        return Some((Token::Number(num_bigfloat::ZERO, decimal_len), len))
    }

    let num = BigFloat::parse(&sliced[cur..len])?;

    Some((Token::Number(num, decimal_len), len))
}

fn try_lex_brace_pipe(input: &str) -> Option<(Token, usize)> {
    let ch = input.chars().next();

    match ch {
        Some('(') => Some((Token::OpenBrace, 1)),
        Some(')') => Some((Token::CloseBrace, 1)),
        _ => None,
    }
}

fn try_lex_comma(input: &str) -> Option<(Token, usize)> {
    let ch = input.chars().next();

    match ch {
        Some(',') => Some((Token::Comma, 1)),
        _ => None,
    }
}

fn try_lex_whitespace(input: &str) -> Option<(Token, usize)> {
    let mut it = input.chars();
    let mut cur = it.next();

    let mut len = 0;

    // while the next char exists, and it is a digit, or it is a decimal point/comma if we have not seen one already
    while let Some(c) = cur {
        if !WHITESPACE.contains(&c) {
            break;
        }

        len += 1;
        cur = it.next();
    }

    if len == 0 {
        return None;
    }

    Some((Token::Whitespace, len))
}

type Lexer = fn(&str) -> Option<(Token, usize)>;

const LEXERS: &'static [Lexer] = &[
    try_lex_whitespace,
    try_lex_let,
    try_lex_equal,
    try_lex_prefix_fn,
    try_lex_other_fn,
    try_lex_math_const,
    try_lex_number,
    try_lex_op,
    try_lex_comma,
    // try_lex_imaginary,
    try_lex_variable,
    try_lex_brace_pipe,
    try_lex_semicolon
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
                        ret.push_back(TokenPos {
                            tk,
                            pos: (pos, pos + len),
                        });
                    }
                    pos += len;
                    progress = true;
                    break;
                }
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
