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
          | Operator String Lisp Lisp
          | Array [Lisp]
          | Ignore
          | End
            deriving (Show)
