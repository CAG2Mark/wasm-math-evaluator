import init, { eval_expr } from "./wasm-math-evaluator/wasm_math_evaluator.js"

await init()

let textArea = document.getElementById("mathinput");
let texout = document.getElementById("texout");
let ans = document.getElementById("ans");

textArea.addEventListener("keyup", (e) => {
    let result = eval_expr(textArea.value);

    console.log(result);

    if (result.parse_success) {
        katex.render(result.latex, texout);
    }

    if (result.eval_success) {
        katex.render(result.text, ans)
    }
})