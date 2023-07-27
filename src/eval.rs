use std::collections::HashMap;

use num_complex::{Complex64, ComplexFloat};

use crate::{ast::{
    tokens::{Operator, Position, PrefixFn},
    tree::{Expr, ExprPos},
}, util::gamma};

type VariableMap = HashMap<char, Complex64>;

impl ExprPos {
    pub fn eval(&self, vars: &VariableMap) -> Result<Complex64, EvalError> {
        self.expr.eval(vars)
    }
}

pub enum EvalError {
    DivByZero(Position),
    ModByZero(Position),
}

impl Expr {
    fn eval(&self, vars: &VariableMap) -> Result<Complex64, EvalError> {
        match &self {
            Expr::Number(n) => Ok(n.into()),
            Expr::ImaginaryConst => Ok(Complex64 { re: 0.0, im: 1.0 }),
            Expr::InfixOp(op, lhs, rhs) => {
                let lhs_e = lhs.eval(vars)?;
                let rhs_e = rhs.eval(vars)?;

                let ret = match op {
                    Operator::Plus => lhs_e + rhs_e,
                    Operator::Minus => lhs_e - rhs_e,
                    Operator::Times => lhs_e * rhs_e,
                    Operator::Div => {
                        if rhs_e.re == 0.0 && rhs_e.im == 0.0 {
                            return Err(EvalError::DivByZero(rhs.pos))
                        }
                        lhs_e / rhs_e
                    }
                    Operator::Mod => {
                        if rhs_e.re == 0.0 && rhs_e.im == 0.0 {
                            return Err(EvalError::ModByZero(rhs.pos))
                        }
                        lhs_e % rhs_e
                    }
                    Operator::Factorial => unreachable!(),
                    Operator::Pow => lhs_e.powc(rhs_e),
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
                        if evaled.re == 0.0 && evaled.im == 0.0 {
                            Complex64 { re: 0.0, im: 0.0 }
                        } else {
                            evaled / evaled.abs()
                        }
                    }
                    PrefixFn::Floor => Complex64 {
                        re: f64::floor(evaled.re),
                        im: f64::floor(evaled.im),
                    },
                    PrefixFn::Ceil => Complex64 {
                        re: f64::ceil(evaled.re),
                        im: f64::ceil(evaled.im),
                    },
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
                    Operator::Factorial => Ok(gamma::gamma(evaled + 1.0)),
                    _ => unreachable!(),
                }
            }
            Expr::Variable(_) => todo!(),
            Expr::OtherFunction(_) => todo!(),
        }
    }
}
