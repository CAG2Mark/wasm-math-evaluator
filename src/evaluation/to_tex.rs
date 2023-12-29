use crate::ast::tokens::MathConst;
use crate::ast::tokens::Operator;
use crate::ast::tokens::OtherFn;
use crate::ast::tokens::PrefixFn;
use crate::ast::tree::Expr;
use crate::ast::tree::ExprPos;
use crate::util::bigfloat2str::bigfloat_to_str;

impl ExprPos {
    pub fn to_tex(&self, lp: u16, rp: u16) -> (String, u8, u8) {
        self.expr.to_tex(lp, rp)
    }
}

impl Expr {
    // (tex output, left multiplication dot priority, right multiplication dot priority.)
    // multiplication dot priority:
    // 0: no need
    // 1: need if the other one needs
    // 2: require. i.e sin x x requires a dot between the two x's, so `sin x`'s right priority is 2
    pub fn to_tex(&self, lp: u16, rp: u16) -> (String, u8, u8) {
        match self {
            Expr::InfixOp(op, lhs, rhs) => {
                match op {
                    Operator::Plus => {
                        // bracket
                        if lp > 1 || rp > 2 {
                            let left = lhs.to_tex(0, 1);
                            let right = rhs.to_tex(2, 0);
                            (
                                "\\left(".to_string() + &left.0 + "+" + &right.0 + "\\right)",
                                0,
                                0,
                            )
                        } else {
                            let left = lhs.to_tex(lp, 1);
                            let right = rhs.to_tex(2, rp);
                            (left.0 + "+" + &right.0, left.1, right.2)
                        }
                    }
                    Operator::Minus => {
                        if lp > 1 || rp > 2 {
                            let left = lhs.to_tex(0, 1);
                            let right = rhs.to_tex(2, 0);
                            (
                                "\\left(".to_string() + &left.0 + "-" + &right.0 + "\\right)",
                                0,
                                0,
                            )
                        } else {
                            let left = lhs.to_tex(lp, 1);
                            let right = rhs.to_tex(2, rp);
                            (left.0 + "-" + &right.0, left.1, right.2)
                        }
                    }
                    Operator::Times => {
                        if lp > 3 || rp > 4 {
                            let left = lhs.to_tex(0, 3);
                            let right = rhs.to_tex(4, 0);

                            let mid = if left.2 + right.1 >= 2 { "\\cdot" } else { "" };
                            (
                                "\\left(".to_string() + &left.0 + mid + " " + &right.0 + "\\right)",
                                0,
                                0,
                            )
                        } else {
                            let left = lhs.to_tex(lp, 3);
                            let right = rhs.to_tex(4, rp);
                            let mid = if left.2 + right.1 >= 2 { "\\cdot" } else { "" };

                            (left.0 + mid + " " + &right.0, left.1, right.2)
                        }
                    }
                    Operator::Div => {
                        // don't care about precedence, will be in a fraction
                        let left = lhs.to_tex(0, 0);
                        let right = rhs.to_tex(0, 0);

                        (format!("\\frac{{{}}}{{{}}}", left.0, right.0), 2, 0)
                    }
                    Operator::Mod => {
                        if lp > 3 || rp > 4 {
                            let left = lhs.to_tex(0, 3);
                            let right = rhs.to_tex(4, 0);
                            (
                                "\\left(".to_string()
                                    + &left.0
                                    + "\\ \\mathrm{mod} \\ "
                                    + &right.0
                                    + "\\right)",
                                0,
                                0,
                            )
                        } else {
                            let left = lhs.to_tex(lp, 3);
                            let right = rhs.to_tex(4, rp);
                            (left.0 + "\\ \\mathrm{mod} \\ " + &right.0, left.1, right.2)
                        }
                    }
                    Operator::Factorial => unreachable!(),
                    Operator::Pow => {
                        let left = lhs.to_tex(lp, 10);
                        // don't care about right precedence
                        let right = rhs.to_tex(0, 0);

                        // if left side is fraction then we should bracket
                        match lhs.expr {
                            Expr::InfixOp(Operator::Div, _, _) => (
                                format!("\\left({}\\right)^{{{}}}", left.0, right.0),
                                0,
                                0,
                            ),
                            _ => (format!("{}^{{{}}}", left.0, right.0), 2, 2),
                        }
                    }
                }
            }
            Expr::PrefixFn(func, expr) => {
                let formatted = match func {
                    PrefixFn::Exp | PrefixFn::Sqrt | PrefixFn::Floor | PrefixFn::Ceil => {
                        expr.to_tex(0, 0)
                    }
                    _ => expr.to_tex(6, rp),
                };

                match func {
                    PrefixFn::Exp => (
                        format!("\\operatorname{{exp}} \\{{{}\\}}", formatted.0),
                        0,
                        2,
                    ),
                    PrefixFn::Sqrt => (format!("\\sqrt{{{}}}", formatted.0), 0, 2),
                    PrefixFn::Floor => (
                        format!("\\left\\lfloor{{{}}}\\right\\rfloor", formatted.0),
                        0,
                        0,
                    ),

                    PrefixFn::Ceil => (
                        format!("\\left\\lceil{{{}}}\\right\\rceil", formatted.0),
                        0,
                        0,
                    ),
                    PrefixFn::Abs => (format!("\\left|{{{}}}\\right|", formatted.0), 0, 0),
                    _ => {
                        let ret = format!("\\operatorname{{{}}}{}", func, formatted.0);
                        if rp > 1 {
                            (format!("\\left({ret}\\right)"), 0, 0)
                        } else {
                            (ret, 0, 2)
                        }
                    }
                }
            }

            Expr::PrefixOp(op, expr) => {
                match op {
                    Operator::Minus => {
                        let formatted = expr.to_tex(2, rp);

                        let ret = format!("-{}", formatted.0);

                        // we usually prefer to always bracket things like -2 if something comes before it
                        if lp > 0 || rp > 6 {
                            ("\\left(".to_string() + &ret + "\\right)", 0, 0)
                        } else {
                            (ret, 2, formatted.2)
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Expr::PostfixOp(op, expr) => match op {
                Operator::Factorial => {
                    let formatted = expr.to_tex(lp, 11);

                    let ret = format!("{}!", formatted.0);
                    if lp > 11 {
                        ("\\left(".to_string() + &ret + "\\right)", 0, 0)
                    } else {
                        (ret, formatted.1, 2)
                    }
                }
                _ => unreachable!(),
            },
            Expr::Variable(chr) => (chr.to_string() + " ", 0, 0),
            Expr::OtherFunction(f, params) => match f {
                OtherFn::Ncr => {
                    let n = params[0].to_tex(0, 0).0;
                    let r = params[1].to_tex(0, 0).0;
                    (format!("\\binom{{{n}}}{{{r}}}"), 0, 0)
                }
                OtherFn::Npr => {
                    let n = params[0].to_tex(0, 0).0;
                    let r = params[1].to_tex(0, 0).0;
                    (format!("P^{{{n}}}_{{{r}}}"), 0, 2)
                }
            },
            Expr::Number(n, d) => {
                let ret = if n.frac().abs() < num_bigfloat::EPSILON {
                    bigfloat_to_str(n, *d)
                } else {
                    bigfloat_to_str(n, *d)
                };

                (ret, 2, 1)
            }
            // Expr::ImaginaryConst => ("i ".to_string(), 0, 0),
            Expr::Const(c) => {
                let ret = match c {
                    MathConst::PI => "\\pi ".to_string(),
                    MathConst::E => "e".to_string(),
                    MathConst::PHI => "\\varphi ".to_string(),
                };
                (ret, 0, 0)
            }
            Expr::Nested(e) => e.to_tex(lp, rp),
        }
    }
}
