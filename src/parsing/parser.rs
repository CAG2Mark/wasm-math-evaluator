use std::collections::VecDeque;

use crate::ast::{
    tokens::{Operator, Position, PrefixFn, Token, TokenPos},
    tree::{Expr, ExprPos},
};

pub enum ParseError {
    UnexpectedToken(Position),
    BraceNotClosed(Position),
    UnexpectedEnd,
    WrongNumArgs(Position),
}

pub enum PrattParseError {
    UnexpectedToken(Position),
    UnexpectedEnd
}

enum ExprOrOp {
    Ex(ExprPos),
    Prefix(PrefixFn, Position),
    Op(Operator, Position),
    PostfixOp(Operator, Position),
}

pub fn parse_expr(tokens: &mut VecDeque<TokenPos>) -> Result<ExprPos, ParseError> {
    let mut l: VecDeque<ExprOrOp> = VecDeque::new();

    // Convert into a list of expressions and tokens/operators.
    while !tokens.is_empty() {
        let first = tokens.front().unwrap();

        match &first.tk {
            Token::Number(_) | Token::Variable(_) | Token::OtherFunction(_) | Token::OpenBrace | Token::ImaginaryConst => {
                l.push_back(ExprOrOp::Ex(parse_atomic(tokens)?));
            }

            Token::PrefixFunction(f) => {
                l.push_back(ExprOrOp::Prefix(*f, first.pos));
                tokens.pop_front();
            }

            Token::Op(Operator::Factorial) => {
                l.push_back(ExprOrOp::PostfixOp(Operator::Factorial, first.pos));
                tokens.pop_front();
            }

            Token::Op(op) => {
                l.push_back(ExprOrOp::Op(*op, first.pos));
                tokens.pop_front();
            }

            Token::CloseBrace => break,

            Token::Whitespace => unreachable!("whitespace seen")
        }
    }

    // Pratt parse.

    let parsed = pratt_parse(&mut l, 0);
    
    match parsed {
        Ok(expr) => Ok(expr),
        Err(err) => match err {
            PrattParseError::UnexpectedToken(pos) => Err(ParseError::UnexpectedToken(pos)),
            PrattParseError::UnexpectedEnd => match tokens.front() {
                Some(tk) => Err(ParseError::UnexpectedToken(tk.pos)),
                None => Err(ParseError::UnexpectedEnd),
            },
        },
    }
}

fn is_infix(op: &Operator) -> bool {
    match op {
        Operator::Plus
        | Operator::Minus
        | Operator::Times
        | Operator::Div
        | Operator::Mod
        | Operator::Pow => true,
        _ => false,
    }
}

fn is_prefix(op: &Operator) -> bool {
    match op {
        Operator::Minus => true,
        _ => false,
    }
}

fn op_prec(op: &Operator) -> (u16, u16) {
    match op {
        Operator::Plus => (1, 2),
        Operator::Minus => (1, 2),
        Operator::Times => (3, 4),
        Operator::Div => (3, 4),
        Operator::Mod => (3, 4),
        Operator::Factorial => (11, 0), // right associativity makes no sense for postfix operators
        Operator::Pow => (9, 10),
    }
}

const FUNC_RIGHT_PREC: u16 = 6;

const IMPLICIT_TIMES_L_PREC: u16 = 3;
const IMPLICIT_TIMES_R_PREC: u16 = 4;

fn pratt_parse(l: &mut VecDeque<ExprOrOp>, prec: u16) -> Result<ExprPos, PrattParseError> {
    let front = l.pop_front();

    match front {
        Some(ExprOrOp::Ex(e)) => pratt_parse_cont(l, e, prec),
        Some(ExprOrOp::Op(op, pos)) if is_prefix(&op) => {
            let rhs = pratt_parse(l, op_prec(&op).1)?;

            let rhs_pos = rhs.pos;

            let val = ExprPos {
                expr: Expr::PrefixOp(op, Box::new(rhs)),
                pos: (pos.0, rhs_pos.1),
            };

            pratt_parse_cont(l, val, prec)
        }
        Some(ExprOrOp::Prefix(f, pos)) => {
            let rhs = pratt_parse(l, FUNC_RIGHT_PREC)?;

            let rhs_pos = rhs.pos;

            let val = ExprPos {
                expr: Expr::PrefixFn(f, Box::new(rhs)),
                pos: (pos.0, rhs_pos.1),
            };

            pratt_parse_cont(l, val, prec)
        }
        Some(ExprOrOp::Op(_, pos) | ExprOrOp::PostfixOp(_, pos)) => {
            Err(PrattParseError::UnexpectedToken(pos))
        }
        _ => Err(PrattParseError::UnexpectedEnd),
    }
}

fn pratt_parse_cont(
    l: &mut VecDeque<ExprOrOp>,
    acc: ExprPos,
    prec: u16,
) -> Result<ExprPos, PrattParseError> {
    match l.front() {
        // infix operators
        Some(ExprOrOp::Op(op, _)) if is_infix(&op) && op_prec(&op).0 > prec => {
            let acc_pos = acc.pos;

            let op_ = *op;

            l.pop_front();

            let rhs = pratt_parse(l, op_prec(&op_).1)?;

            let rhs_pos = rhs.pos;

            pratt_parse_cont(
                l,
                ExprPos {
                    expr: Expr::InfixOp(op_, Box::new(acc), Box::new(rhs)),
                    pos: (acc_pos.0, rhs_pos.1),
                },
                prec,
            )
        }
        // implicit multiplication
        Some(ExprOrOp::Ex(_)) if IMPLICIT_TIMES_L_PREC > prec => {
            let acc_pos = acc.pos;

            let rhs = pratt_parse(l, IMPLICIT_TIMES_R_PREC)?;

            let rhs_pos = rhs.pos;

            pratt_parse_cont(
                l,
                ExprPos {
                    expr: Expr::InfixOp(Operator::Times, Box::new(acc), Box::new(rhs)),
                    pos: (acc_pos.0, rhs_pos.1),
                },
                prec,
            )
        }
        // postfix operators
        Some(ExprOrOp::PostfixOp(op, pos)) if op_prec(&op).0 > prec => {
            let acc_pos = acc.pos;

            let val = ExprPos {
                expr: Expr::PostfixOp(*op, Box::new(acc)),
                pos: (acc_pos.0, pos.1),
            };

            l.pop_front();

            // postfix the accumlated value and continue parsing
            pratt_parse_cont(l, val, prec)
        }
        _ => Ok(acc),
    }
}

// Atomic expressions are single variables, numbers, bracketed exprs, or function calls with more than two arguments.
// Function calls with only one argument are treated like prefix operators.
pub fn parse_atomic(tokens: &mut VecDeque<TokenPos>) -> Result<ExprPos, ParseError> {
    match &tokens.front() {
        Some(tk) => {
            let pos = tk.pos;

            let ret = match tk.tk {
                Token::ImaginaryConst => {
                    tokens.pop_front();
                    ExprPos {
                        expr: Expr::ImaginaryConst,
                        pos
                    }
                }
                Token::Number(n) => {
                    tokens.pop_front();
                    ExprPos {
                        expr: Expr::Number(n),
                        pos,
                    }
                }
                Token::Variable(v) => {
                    tokens.pop_front();
                    ExprPos {
                        expr: Expr::Variable(v),
                        pos,
                    }
                }
                Token::OtherFunction(_) => todo!(),

                Token::OpenBrace => {
                    tokens.pop_front();

                    let ret = parse_expr(tokens)?;

                    let end_pos = match tokens.pop_front() {
                        Some(tk) if matches!(tk.tk, Token::CloseBrace) => tk.pos,
                        _ => return Err(ParseError::BraceNotClosed(pos)),
                    };

                    ExprPos {
                        expr: ret.expr,
                        pos: (pos.0, end_pos.1),
                    }
                }

                Token::Op(_) | Token::PrefixFunction(_) | Token::CloseBrace => {
                    return Err(ParseError::UnexpectedToken(pos))
                }

                Token::Whitespace => unreachable!("whitespace detected"),
            };

            Ok(ret)
        }
        None => Err(ParseError::UnexpectedEnd),
    }
}
