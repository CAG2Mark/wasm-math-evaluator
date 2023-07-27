mod utils;

pub mod ast;
pub mod parsing;
pub mod util;
pub mod eval;

use std::collections::HashMap;

use eval::EvalError;
use num_complex::Complex64;
use parsing::parser::ParseError;
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
        EvalError::DivByZero(pos) => ("division by zero", pos),
        EvalError::ModByZero(pos) => ("mod by zero", pos),
    };

    ErrorInfo {
        msg: msg.to_string(), start: pos.0, len: pos.1 - pos.0
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

    let parsed = parsing::parser::parse_expr(&mut tokens);

    match parsed {
        Ok(expr) => console_log!("{}", expr),
        Err(err) => print_error(inp, err)
    }
}


// for binding purpose
#[derive(Serialize, Deserialize)]
pub struct EvalErrWrap {
    success: bool, // always false
    error: ErrorInfo, // error msg, start pos, length
}

#[derive(Serialize, Deserialize)]
pub struct EvalSuccessWrap {
    success: bool, // always true
    re: f64,
    im: f64
}

#[wasm_bindgen]
pub fn eval_expr(inp: &str) -> JsValue {
    let mut tokens = match parsing::lexer::lex(inp) {
        Ok(tks) => tks,
        Err(pos) => {
            let err = ErrorInfo {
                msg: "unexpected token".to_string(),
                start: pos,
                len: 1
            };

            let res = EvalErrWrap {
                success: false,
                error: err
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    let parsed = match parsing::parser::parse_expr(&mut tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let err = parse_error_to_info(err, inp.len());

            let res = EvalErrWrap {
                success: false,
                error: err
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    };

    match parsed.eval(&HashMap::new()) {
        Ok(val) => {
            let res = EvalSuccessWrap {
                success: true,
                re: val.re,
                im: val.im
            };

            serde_wasm_bindgen::to_value(&res).unwrap()
        }
        Err(err) => {
            let err = eval_error_to_info(err);

            let res = EvalErrWrap {
                success: false,
                error: err
            };
            return serde_wasm_bindgen::to_value(&res).unwrap();
        }
    }
}


#[wasm_bindgen]
pub fn add(x: u64, y: u64) -> u64 {
    console_log!("{} + {} = {}", x, y, x + y);
    x + y
}