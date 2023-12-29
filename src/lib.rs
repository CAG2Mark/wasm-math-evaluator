mod utils;

pub mod ast;
pub mod parsing;
pub mod util;
pub mod evaluation;

use std::collections::HashMap;

use evaluation::eval::EvalError;
use num_bigfloat::BigFloat;
use parsing::parser::ParseError;
use util::bigfloat2str::bigfloat_auto_str;
use utils::set_panic_hook;
use wasm_bindgen::prelude::*;

use serde::{Serialize, Deserialize};

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

#[wasm_bindgen]
pub fn greet() {
    alert("Hello, wasm-math!");
}

#[wasm_bindgen]
pub fn lex_eqn(inp: &str) {
    let tokens = parsing::lexer::lex(inp);

    match tokens {
        Ok(tks) => tks.iter().for_each(|t| console_log!("{}", t)),
        Err(pos) => console_log!("unexpected token at position {}", pos),
    }
}


#[derive(Serialize, Deserialize)]
pub struct ErrorInfo {
    pub msg: String,
    pub start: usize,
    pub len: usize
}

// message, spaces, caretes
fn parse_error_to_info(err: ParseError, inp_len: usize) -> ErrorInfo {
    let (msg, start, len) = match err {
        ParseError::UnexpectedToken(pos) => (
            "unexpected token", pos.0, pos.1 - pos.0
        ),
        ParseError::BraceNotClosed(pos) => (
            "unclosed parentheses", pos.0, pos.1 - pos.0
        ),
        ParseError::UnexpectedEnd => (
            "unexpected end", inp_len - 1, 1
        ),
        ParseError::WrongNumArgs(pos) => (
            "wrong number of arguments", pos.0, pos.1 - pos.0
        ),
    };

    ErrorInfo {
        msg: msg.to_string(), start, len
    }
}

fn eval_error_to_info(err: EvalError) -> ErrorInfo {
    let (msg, pos) = match err {
        EvalError::DivByZero(pos) => ("division by zero".to_string(), pos),
        EvalError::ModByZero(pos) => ("mod by zero".to_string(), pos),
        EvalError::VariableNotFound(var_name, pos) => (format!("variable {var_name} not found"), pos)
    };

    ErrorInfo {
        msg: msg, start: pos.0, len: pos.1 - pos.0
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
pub fn parse_eqn(inp: &str) {
    // set_panic_hook();

    let mut tokens = match parsing::lexer::lex(inp) {
        Ok(tks) => tks,
        Err(pos) => {
            console_log!("unexpected token at position {}", pos);
            return;
        }
    };

    let parsed = parsing::parser::parse(&mut tokens);

    match parsed {
        Ok(expr) => console_log!("{}", expr),
        Err(err) => print_error(inp, err)
    }
}


// for binding purpose
#[derive(Serialize, Deserialize)]
pub struct ErrWrap {
    success: bool, // always false
    error: ErrorInfo, // error msg, start pos, length
}

#[derive(Serialize, Deserialize)]
pub struct EvalSuccessWrap {
    success: bool, // always true
    latex: String,
    is_nan: bool,
    is_inf: bool,
    mantissa: [i16; 10],
    exp: i8,
    sign: i8,
    text: String
}

#[wasm_bindgen]
pub fn eval_expr(inp: &str) -> JsValue {
    set_panic_hook();
    
    let mut tokens = match parsing::lexer::lex(inp) {
        Ok(tks) => tks,
        Err(pos) => {
            let err = ErrorInfo {
                msg: "unexpected token".to_string(),
                start: pos,
                len: 1
            };

            let res = ErrWrap {
                success: false,
                error: err
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let parsed = match parsing::parser::parse(&mut tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let err = parse_error_to_info(err, inp.len());

            let res = ErrWrap {
                success: false,
                error: err
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    match parsed.eval(&HashMap::new()) {
        Ok(val) => {
            let res = match val.to_raw_parts() {
                Some((mantissa, _dp, sign, exp)) => {
                    EvalSuccessWrap {
                        success: true,
                        latex: parsed.to_tex(0, 0).0,
                        is_nan: false,
                        is_inf: false,
                        mantissa: mantissa,
                        exp: exp,
                        sign: sign,
                        text: bigfloat_auto_str(&val)
                    }
                }
                None => {
                    let zeros = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
                    if val.is_nan() {
                        EvalSuccessWrap {
                            success: true,
                            latex: parsed.to_tex(0, 0).0,
                            is_nan: true,
                            is_inf: false,
                            mantissa: zeros,
                            exp: 0,
                            sign: 0,
                            text: val.to_string()
                        }
                    } else if val.is_inf() {
                        EvalSuccessWrap {
                            success: true,
                            latex: parsed.to_tex(0, 0).0,
                            is_nan: false,
                            is_inf: true,
                            mantissa: zeros,
                            exp: val.get_sign(),
                            sign: 0,
                            text: val.to_string()
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

            let res = ErrWrap {
                success: false,
                error: err
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct TexSuccessWrap {
    success: bool, // always true
    val: String
}

#[wasm_bindgen]
pub fn to_tex(inp: &str) -> JsValue {
    let mut tokens = match parsing::lexer::lex(inp) {
        Ok(tks) => tks,
        Err(pos) => {
            let err = ErrorInfo {
                msg: "unexpected token".to_string(),
                start: pos,
                len: 1
            };

            let res = ErrWrap {
                success: false,
                error: err
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let parsed = match parsing::parser::parse(&mut tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let err = parse_error_to_info(err, inp.len());

            let res = ErrWrap {
                success: false,
                error: err
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let tex = parsed.to_tex(0, 0);

    let res = TexSuccessWrap {
        success: true,
        val: tex.0
    };

    serde_wasm_bindgen::to_value(&res).unwrap()
}