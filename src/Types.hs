module Types where

data Lisp = Invocation Lisp [Lisp]
          | Identifier String
          | StringLit String
          | IntLit Int
          | Lambda [Lisp] [Lisp]
          | Ignore
          | End
            deriving (Show)
