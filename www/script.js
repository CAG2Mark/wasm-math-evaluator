import init, { eval_expr } from "./wasm-math-evaluator/wasm_math_evaluator.js"

await init()

let textArea = document.getElementById("mathinput");
let texout = document.getElementById("texout");
let ans = document.getElementById("ans");

textArea.addEventListener("keyup", (e) => {
    let result = eval_expr(textArea.value);

    console.log(result);

    if (result.EvalError) {
        katex.render(result.EvalError.latex, texout);
    } else if (result.EvalSuccess) {
        katex.render(result.EvalSuccess.latex, texout);
        katex.render(result.EvalSuccess.text, ans)
    }
})