import init, { eval_expr } from "./wasm-math-evaluator/wasm_math_evaluator.js"

await init()

let textArea = document.getElementById("mathinput");
let texout = document.getElementById("texout");
let ans = document.getElementById("ans");

textArea.addEventListener("keyup", (e) => {
    let res = eval_expr(textArea.value);

    console.log(res);

    if (res.result == "EvalError") {
        katex.render(res.latex, texout);
    } else if (res.result == "EvalSuccess") {
        katex.render(res.latex, texout);
        katex.render(res.text, ans)
    }
})

function test() {
    let res = eval_expr(textArea.value);
    console.log(res);
}