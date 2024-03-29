# lispscript

![Circle CI](https://img.shields.io/circleci/project/github/swissChili/lispscript.svg)

Lispscript is a lisp dialect that compiles to JavaScript. It is written in
Haskell and uses the Parsec library of parser combinators. It's syntax is
similar to most Lisp-like languages, with some small diferences to make it
more modern and work better when compiled to JS.

```clj
(let fs (require "fs"))

; Define a function `foo`
(let foo [a] => (do
  (let file (fs.readFileSync a "utf-8"))
  (console.log (+ a "\n\n" file))
))

(foo "hello.lss")
```

## installation

Either build from source using the haskell stack or install a binary from
the releases page.

```sh
$ stack install
Copied executables to /home/user/.local/bin:
- lsc
$ lsc hello.lss | node
Hello, World!
```

## syntax

Lispscript uses s-expressions for almost everything (with the exception
of lambdas). There are no statements, only functions.

```clj
; This is a function call
(foo "bar")
```
There are several reserved functions that compile to special syntax in
JavaScript. They are listed bellow:

- `(let name value)`: Binds a value to a name. Compiles to `var name = value`
  in JavaScript.
- `(set name value)`: Sets a value to a name. Compiles to `name = value` in
  JavaScript
- `(do body...)`: Same as a `do` block in Haskell, or the `do` function in
  other Lisps. Useful for chaining multiple statements together. Often
  paired with a lambda.

### Lambdas

Lambdas are one of the exceptions to the s-expression syntax.

```clj
[a b] => (.log console "A is" a "B is" b)
```
They are similar to JavaScript lambdas except they use square brackets instead
of parenthesis.

Methods in Lispscript use polish notation. This means they are written with the
method first, not the object it is called on. For example, the following JS
code:
```js
console.log("hello there");
```
Would be written like so:
```clj
(.log console "hello there")
```
However, Lispscript also supports using both the object and the method in the
invocation as a shorthand. However the polish notation method is recommended
since it alows one to call methods on anonymous objects.

### Objects

Objects in lispscript are fairly similar to other lisp dialects, and quite
different from Javascript. Objects are defined with square braces and indexed
with the usual S-expressions. 
```clj
(let foo "last value")
(let obj
  [ :key "val"
    :anotherKey 123
    :yetAnotherKey foo ])
(.log console (:key obj))
;=> val
```
