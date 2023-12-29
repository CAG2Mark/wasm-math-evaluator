import init, { lex_eqn, parse_eqn, eval_expr, to_tex } from "./wasm-math/wasm_math.js"

await init()

function eval_and_print(expr) {
    let val = eval_expr(expr);
    console.log(val)
}

// lex_eqn("1+arcsin(2+2.000)! * sinx!")

// parse_eqn("sin(2i)")

// eval_expr("e^(pi i)")
eval_and_print("sqrt(-1)")

let textArea = document.getElementById("mathinput");
let texout = document.getElementById("texout");
let ans = document.getElementById("ans");

textArea.addEventListener("keyup", (e) => {
    let result = to_tex(textArea.value);

    console.log(result);

    if (result.success) {
        katex.render(result.val, texout);
    }

    let answer = eval_expr(textArea.value);

    console.log(answer);

    if (answer.success) {
        katex.render(answer.text, ans)
    }
})