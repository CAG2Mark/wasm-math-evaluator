use std::collections::HashMap;

use std::f64::consts;

use num_bigfloat::BigFloat;

use crate::{ast::{
    tokens::{Operator, Position, PrefixFn, MathConst},
    tree::{Expr, ExprPos},
}, util::gamma};

type VariableMap = HashMap<char, BigFloat>;

impl ExprPos {
    pub fn eval(&self, vars: &VariableMap) -> Result<BigFloat, EvalError> {
        self.expr.eval(vars)
    }
}

pub enum EvalError {
    DivByZero(Position),
    ModByZero(Position),
}

impl Expr {
    fn eval(&self, vars: &VariableMap) -> Result<BigFloat, EvalError> {
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
                    Operator::Div => {
                        if rhs_e.is_zero() {
                            return Err(EvalError::DivByZero(rhs.pos))
                        }
                        lhs_e / rhs_e
                    }
                    Operator::Mod => lhs_e % rhs_e,
                    Operator::Factorial => unreachable!(),
                    Operator::Pow => lhs_e.pow(&rhs_e)
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
                    PrefixFn::Sgn => if evaled > BigFloat::from(0) {
                        BigFloat::from(1)
                    } else if evaled.is_zero() {
                        BigFloat::from(0)
                    } else {
                        BigFloat::from(-1)
                    }
                    PrefixFn::Floor => evaled.floor(),
                    PrefixFn::Ceil => evaled.ceil(),
                    PrefixFn::Abs => evaled.abs()
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
                    Operator::Factorial => Ok(gamma::gamma_spouge(evaled + num_bigfloat::ONE)),
                    _ => unreachable!(),
                }
            }
            Expr::Variable(_) => todo!(),
            Expr::OtherFunction(_) => todo!(),
            Expr::Const(c) => match c {
                MathConst::PI => Ok(num_bigfloat::PI),
                MathConst::E => Ok(num_bigfloat::E),
                MathConst::PHI => Ok((num_bigfloat::ONE + BigFloat::from(5).sqrt()) / num_bigfloat::TWO),
            },
            Expr::Nested(e) => e.eval(vars),
        }
    }
}