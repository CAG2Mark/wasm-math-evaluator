mod utils;

pub mod ast;
pub mod evaluation;
pub mod parsing;
pub mod util;

use std::collections::HashMap;

use evaluation::eval::EvalError;
use parsing::parser::ParseError;
use util::bigfloat_utils::{bigfloat_auto_str, get_exponent, mantissa_tostr, get_decimal_places};
use utils::set_panic_hook;
use wasm_bindgen::prelude::*;

use serde::{Deserialize, Serialize};

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    // The `console.log` is quite polymorphic, so we can bind it with multiple
    // signatures. Note that we need to use `js_name` to ensure we always call
    // `log` in JS.
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_u32(a: u32);

    // Multiple arguments too!
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_many(a: &str, b: &str);
}

macro_rules! console_log {
    // Note that this is using the `log` function imported above during
    // `bare_bones`
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[derive(Serialize, Deserialize)]
pub struct ErrorInfo {
    pub msg: String,
    pub start: usize,
    pub len: usize,
}

// message, spaces, caretes
fn parse_error_to_info(err: ParseError, inp_len: usize) -> ErrorInfo {
    let (msg, start, len) = match err {
        ParseError::UnexpectedToken(pos) => ("unexpected token", pos.0, pos.1 - pos.0),
        ParseError::BraceNotClosed(pos) => ("unclosed parentheses", pos.0, pos.1 - pos.0),
        ParseError::UnexpectedEnd => ("unexpected end", inp_len - 1, 1),
        ParseError::WrongNumArgs(pos) => ("wrong number of arguments", pos.0, pos.1 - pos.0),
    };

    ErrorInfo {
        msg: msg.to_string(),
        start,
        len,
    }
}

fn eval_error_to_info(err: EvalError) -> ErrorInfo {
    let (msg, pos) = match err {
        EvalError::ModByZero(pos) => ("mod by zero".to_string(), pos),
        EvalError::VariableNotFound(var_name, pos) => {
            (format!("variable {var_name} not found"), pos)
        }
    };

    ErrorInfo {
        msg: msg,
        start: pos.0,
        len: pos.1 - pos.0,
    }
}

pub fn print_error(inp: &str, err: ParseError) {
    let info = parse_error_to_info(err, inp.len());

    let spaces = format!("{: <1$}", "", info.start);
    let carets = format!("{:^<1$}", "", info.len);

    let out = format!("{}\n{}{}\n{}", inp, spaces, carets, info.msg);

    console_log!("{}", out);
}

pub fn print_eval_err(inp: &str, err: EvalError) {
    let info = eval_error_to_info(err);

    let spaces = format!("{: <1$}", "", info.start);
    let carets = format!("{:^<1$}", "", info.len);

    let out = format!("{}\n{}{}\n{}", inp, spaces, carets, info.msg);

    console_log!("{}", out);
}

#[derive(Serialize, Deserialize)]
pub struct EvalWrap {
    parse_success: bool,
    eval_success: bool,
    latex: String,
    is_nan: bool,
    is_inf: bool,
    mantissa: String,
    exp: i64,
    sign: i8,
    is_exact: bool,
    text: String,
    error: ErrorInfo,
}

#[wasm_bindgen]
pub fn eval_expr(inp: &str) -> JsValue {
    set_panic_hook();

    let zeros = "".to_string();

    let mut tokens = match parsing::lexer::lex(inp) {
        Ok(tks) => tks,
        Err(pos) => {
            let err = ErrorInfo {
                msg: "unexpected token".to_string(),
                start: pos,
                len: 1,
            };

            let res = EvalWrap {
                parse_success: false,
                eval_success: false,
                latex: "".to_string(),
                is_nan: false,
                is_inf: false,
                is_exact: false,
                mantissa: zeros,
                exp: 0,
                sign: 0,
                text: "".to_string(),
                error: err,
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let parsed = match parsing::parser::parse(&mut tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let err = parse_error_to_info(err, inp.len());

            let res = EvalWrap {
                parse_success: false,
                eval_success: false,
                latex: "".to_string(),
                is_nan: false,
                is_inf: false,
                is_exact: false,
                mantissa: zeros,
                exp: 0,
                sign: 0,
                text: "".to_string(),
                error: err,
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let empty_error = ErrorInfo {
        msg: "".to_string(),
        start: 0,
        len: 0,
    };

    match parsed.eval(&HashMap::new()) {
        Ok(val) => {
            let res = match val.to_raw_parts() {
                Some((mantissa, _dp, sign, _exp)) => EvalWrap {
                    parse_success: true,
                    eval_success: true,
                    latex: parsed.to_tex(0, 0).0,
                    is_nan: false,
                    is_inf: false,
                    is_exact: val.is_zero() || (get_exponent(&val) + get_decimal_places(&val) as i64) < 39,
                    mantissa: mantissa_tostr(mantissa, true),
                    exp: if val.is_zero() { 0 } else { get_exponent(&val) },
                    sign: sign,
                    text: bigfloat_auto_str(&val),
                    error: empty_error,
                },
                None => {
                    if val.is_nan() {
                        EvalWrap {
                            parse_success: true,
                            eval_success: true,
                            latex: parsed.to_tex(0, 0).0,
                            is_nan: true,
                            is_inf: false,
                            is_exact: false,
                            mantissa: zeros,
                            exp: 0,
                            sign: 0,
                            text: val.to_string(),
                            error: empty_error,
                        }
                    } else if val.is_inf() {
                        EvalWrap {
                            parse_success: true,
                            eval_success: true,
                            latex: parsed.to_tex(0, 0).0,
                            is_nan: false,
                            is_inf: true,
                            is_exact: false,
                            mantissa: zeros,
                            exp: 0,
                            sign: val.get_sign(),
                            text: val.to_string(),
                            error: empty_error,
                        }
                    } else {
                        unreachable!()
                    }
                }
            };

            serde_wasm_bindgen::to_value(&res).unwrap()
        }
        Err(err) => {
            let err = eval_error_to_info(err);

            let res = EvalWrap {
                parse_success: true,
                eval_success: false,
                latex: parsed.to_tex(0, 0).0,
                is_nan: false,
                is_inf: true,
                is_exact: false,
                mantissa: zeros,
                exp: 0,
                sign: 0,
                text: "".to_string(),
                error: err,
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    }
}
