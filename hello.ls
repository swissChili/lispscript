(let fs (require "fs"))

; Define a function `foo`
(let foo [a] : {
  (let file (fs.readFileSync "README.md" "utf-8"))
  (console.log (+ "README\n\n" file))
})

(foo "hey")
