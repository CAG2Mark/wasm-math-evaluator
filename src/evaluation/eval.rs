use num_bigfloat::BigFloat;
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        tokens::{MathConst, Operator, OtherFn, Position, PrefixFn},
        tree::{Expr, ExprPos},
    },
    util::{
        combinatorics::{ncr, npr},
        gamma, atan2::atan2,
    },
};

type VariableMap = HashMap<char, BigFloat>;
type VariableSet = HashSet<char>;

impl ExprPos {
    pub fn eval(&self, vars: &mut VariableMap) -> Result<BigFloat, EvalError> {
        self.expr.eval(vars, &self.pos)
    }
    pub fn check(&self, vars: &mut VariableSet) -> Result<(), EvalError> {
        self.expr.check(vars, &self.pos)
    }
}

pub enum EvalError {
    VariableNotFound(String, Position),
    VariableAlreadyDefined(String, Position),
}

impl Expr {
    fn eval(&self, vars: &mut VariableMap, pos: &Position) -> Result<BigFloat, EvalError> {
        match &self {
            Expr::Number(n, _) => Ok(*n),
            // Expr::ImaginaryConst => Ok(Complex64 { re: 0.0, im: 1.0 }),
            Expr::InfixOp(op, lhs, rhs) => {
                let lhs_e = lhs.eval(vars)?;
                let rhs_e = rhs.eval(vars)?;

                let ret = match op {
                    Operator::Plus => lhs_e + rhs_e,
                    Operator::Minus => lhs_e - rhs_e,
                    Operator::Times => lhs_e * rhs_e,
                    Operator::Div => lhs_e / rhs_e,
                    Operator::Mod => lhs_e % rhs_e,
                    Operator::Factorial => unreachable!(),
                    Operator::Pow => if lhs_e.is_zero() && rhs_e.is_zero() {
                        num_bigfloat::ONE
                    } else {
                        lhs_e.pow(&rhs_e)
                    }
                };

                Ok(ret)
            }
            Expr::PrefixFn(func, expr) => {
                let evaled = expr.eval(vars)?;

                let ret = match func {
                    PrefixFn::Sin => evaled.sin(),
                    PrefixFn::Cos => evaled.cos(),
                    PrefixFn::Tan => evaled.tan(),
                    PrefixFn::Arcsin => evaled.asin(),
                    PrefixFn::Arccos => evaled.acos(),
                    PrefixFn::Arctan => evaled.atan(),
                    PrefixFn::Sinh => evaled.sinh(),
                    PrefixFn::Cosh => evaled.cosh(),
                    PrefixFn::Tanh => evaled.tanh(),
                    PrefixFn::Arsinh => evaled.asinh(),
                    PrefixFn::Arcosh => evaled.acosh(),
                    PrefixFn::Artanh => evaled.atanh(),
                    PrefixFn::Exp => evaled.exp(),
                    PrefixFn::Ln => evaled.ln(),
                    PrefixFn::Sqrt => evaled.sqrt(),
                    PrefixFn::Sgn => {
                        if evaled > BigFloat::from(0) {
                            BigFloat::from(1)
                        } else if evaled.is_zero() {
                            BigFloat::from(0)
                        } else {
                            BigFloat::from(-1)
                        }
                    }
                    PrefixFn::Floor => evaled.floor(),
                    PrefixFn::Ceil => evaled.ceil(),
                    PrefixFn::Abs => evaled.abs(),
                };

                Ok(ret)
            }
            Expr::PrefixOp(op, expr) => {
                let evaled = expr.eval(vars)?;

                let ret = match op {
                    Operator::Minus => -evaled,
                    _ => unreachable!(),
                };

                Ok(ret)
            }
            Expr::PostfixOp(op, expr) => {
                let evaled = expr.eval(vars)?;

                match op {
                    Operator::Factorial => Ok(gamma::factorial(&evaled)),
                    _ => unreachable!(),
                }
            }
            Expr::Variable(v) => match vars.get(v) {
                Some(val) => Ok(*val),
                None => Err(EvalError::VariableNotFound(v.to_string(), *pos)),
            },
            Expr::OtherFunction(f, params) => match f {
                OtherFn::Ncr => {
                    let n = params[0].eval(vars)?;
                    let r = params[1].eval(vars)?;
                    Ok(ncr(&n, &r))
                }
                OtherFn::Npr => {
                    let n = params[0].eval(vars)?;
                    let r = params[1].eval(vars)?;
                    Ok(npr(&n, &r))
                }
                OtherFn::Atan2 => {
                    let y = params[0].eval(vars)?;
                    let x = params[1].eval(vars)?;
                    Ok(atan2(&y, &x))
                },
                OtherFn::Min => {
                    let a = params[0].eval(vars)?;
                    let b = params[1].eval(vars)?;
                    Ok(a.min(&b))
                },
                OtherFn::Max => {
                    let a = params[0].eval(vars)?;
                    let b = params[1].eval(vars)?;
                    Ok(a.max(&b))
                },
            },
            Expr::Const(c) => match c {
                MathConst::PI => Ok(num_bigfloat::PI),
                MathConst::E => Ok(num_bigfloat::E),
                MathConst::PHI => {
                    Ok((num_bigfloat::ONE + BigFloat::from(5).sqrt()) / num_bigfloat::TWO)
                }
            },
            Expr::Nested(e) => e.eval(vars),
            Expr::Let(ch, ch_pos, body, rest) => {
                match vars.get(ch) {
                    Some(_) => Err(EvalError::VariableAlreadyDefined(ch.to_string(), *ch_pos)),
                    None => {
                        let body_e = body.eval(vars)?;

                        vars.insert(*ch, body_e);
                        let ret = rest.eval(vars)?;
                        vars.remove(ch);

                        Ok(ret)
                    },
                }
            },
        }
    }

    fn check(&self, vars: &mut VariableSet, pos: &Position) -> Result<(), EvalError> {
        match self {
            Expr::InfixOp(_, lhs, rhs) => {
                lhs.check(vars)?;
                rhs.check(vars)
            }
            Expr::PrefixFn(_, operand) => operand.check(vars),
            Expr::PrefixOp(_, operand) => operand.check(vars),
            Expr::PostfixOp(_, operand) => operand.check(vars),
            Expr::Variable(nme) => match vars.get(nme) {
                Some(_) => Ok(()),
                None => Err(EvalError::VariableNotFound(nme.to_string(), *pos)),
            },
            Expr::Nested(e) => e.check(vars),
            Expr::Const(_) => Ok(()),
            Expr::OtherFunction(_, exprs) => {
                for expr in exprs {
                    expr.check(vars)?
                }
                Ok(())
            }
            Expr::Number(_, _) => Ok(()),
            Expr::Let(ch, ch_pos, body, rest) => {
                match vars.get(ch) {
                    Some(_) => Err(EvalError::VariableAlreadyDefined(ch.to_string(), *ch_pos)),
                    None => {
                        body.check(vars)?;

                        vars.insert(*ch);
                        rest.check(vars)?;
                        vars.remove(ch);

                        Ok(())
                    },
                }
            },
        }
    }
}
