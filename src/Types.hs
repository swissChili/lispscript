module Types where

data Lisp = Invocation Lisp [Lisp]
          | Method Lisp Lisp [Lisp]
          | DoBlock [Lisp]
          | Identifier String
          | StringLit String
          | IntLit Int
          | Lambda [Lisp] Lisp
          | Binding Lisp Lisp
          | Assignment Lisp Lisp
          | Operator String [Lisp]
          | Constructor Lisp [Lisp]
          | KeyValPair Lisp Lisp
          | Object [Lisp]
          | ObjIndex Lisp Lisp
          | ObjIndexVar Lisp Lisp
          | IfStatement Lisp Lisp Lisp
          | Array [Lisp]
          | Return Lisp
          | Ignore
          | BoolTrue
          | BoolFalse
          | End
            deriving (Show)
