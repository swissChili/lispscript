# lispscript

Lispscript is a lisp dialect that compiles to JavaScript. It is written in
Haskell and uses the Parsec library of parser combinators. It's syntax is
similar to most Lisp-like languages, with some small diferences to make it
more modern and work better when compiled to JS.

```clj
(let fs (require "fs"))

; Define a function `foo`
(let foo [a] => (do
  (let file (fs.readFileSync a "utf-8"))
  (console.log (+ (+ a "\n\n") file))
))

(foo "hello.lss")
```
