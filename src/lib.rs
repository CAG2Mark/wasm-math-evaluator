mod utils;

pub mod ast;
pub mod evaluation;
pub mod parsing;
pub mod util;

use std::collections::{HashMap, HashSet};

use evaluation::eval::EvalError;
use parsing::parser::ParseError;
use util::bigfloat_utils::{bigfloat_auto_str, get_decimal_places, get_exponent, mantissa_tostr};
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

#[derive(Serialize, Deserialize)]
#[serde(tag = "result")]
pub enum EvalResult {
    EvalSuccess {
        latex: String,
        is_nan: bool,
        is_inf: bool,
        mantissa: String,
        exp: i64,
        sign: i8,
        is_exact: bool,
        text: String,
        raw: [i16; 10],
    },
    CheckSuccess {
        latex: String,
    },
    EvalError {
        error: ErrorInfo,
        latex: String,
    },
    ParseError {
        error: ErrorInfo,
    },
}

// message, spaces, caretes
fn parse_error_to_info(err: ParseError, inp_len: usize) -> ErrorInfo {
    let (msg, start, len) = match err {
        ParseError::UnexpectedToken(pos) => ("unexpected token", pos.0, pos.1 - pos.0),
        ParseError::BraceNotClosed(pos) => ("unclosed parentheses", pos.0, pos.1 - pos.0),
        ParseError::UnexpectedEnd => ("unexpected end", inp_len, 1),
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

#[wasm_bindgen]
pub fn check_expr(inp: &str, variables: JsValue) -> JsValue {
    set_panic_hook();

    let vars: Vec<String> = match serde_wasm_bindgen::from_value(variables) {
        Ok(v) => v,
        Err(_) => return serde_wasm_bindgen::to_value(&()).unwrap(),
    };

    let mut tokens = match parsing::lexer::lex(inp) {
        Ok(tks) => tks,
        Err(pos) => {
            let err = ErrorInfo {
                msg: "unexpected token".to_string(),
                start: pos,
                len: 1,
            };

            let res = EvalResult::ParseError { error: err };

            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let parsed = match parsing::parser::parse(&mut tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let err = parse_error_to_info(err, inp.len());

            let res = EvalResult::ParseError { error: err };

            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let mut var_set: HashSet<char> = HashSet::new();
    for s in vars {
        if s.len() > 1 {
            return serde_wasm_bindgen::to_value(&()).unwrap();
        }

        match s.chars().next() {
            Some(c) => { var_set.insert(c); },
            None => return serde_wasm_bindgen::to_value(&()).unwrap(),
        }
    }

    let res = match parsed.check(&var_set) {
        Ok(_) => EvalResult::CheckSuccess {
            latex: parsed.to_tex(0, 0).0,
        },
        Err(err) => EvalResult::EvalError {
            error: eval_error_to_info(err),
            latex: parsed.to_tex(0, 0).0,
        },
    };

    serde_wasm_bindgen::to_value(&res).unwrap()
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

            let res = EvalResult::ParseError { error: err };

            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let parsed = match parsing::parser::parse(&mut tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let err = parse_error_to_info(err, inp.len());

            let res = EvalResult::ParseError { error: err };

            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let zeros_raw = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    match parsed.eval(&HashMap::new()) {
        Ok(val) => {
            let res = match val.to_raw_parts() {
                Some((mantissa, _dp, sign, _exp)) => EvalResult::EvalSuccess {
                    latex: parsed.to_tex(0, 0).0,
                    is_nan: false,
                    is_inf: false,
                    is_exact: val.is_zero()
                        || (get_exponent(&val) + get_decimal_places(&val) as i64) < 39,
                    mantissa: mantissa_tostr(mantissa, true),
                    exp: if val.is_zero() { 0 } else { get_exponent(&val) },
                    sign: sign,
                    text: bigfloat_auto_str(&val),
                    raw: mantissa,
                },
                None => {
                    if val.is_nan() {
                        EvalResult::EvalSuccess {
                            latex: parsed.to_tex(0, 0).0,
                            is_nan: true,
                            is_inf: false,
                            is_exact: false,
                            mantissa: zeros,
                            exp: 0,
                            sign: 0,
                            text: bigfloat_auto_str(&val),
                            raw: zeros_raw,
                        }
                    } else if val.is_inf() {
                        EvalResult::EvalSuccess {
                            latex: parsed.to_tex(0, 0).0,
                            is_nan: false,
                            is_inf: true,
                            is_exact: false,
                            mantissa: zeros,
                            exp: 0,
                            sign: val.get_sign(),
                            text: bigfloat_auto_str(&val),
                            raw: zeros_raw,
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

            let res = EvalResult::EvalError {
                error: err,
                latex: parsed.to_tex(0, 0).0,
            };

            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    }
}
