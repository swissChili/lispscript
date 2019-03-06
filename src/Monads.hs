module Monads where

(|>) :: a -> (a -> b) -> b
a |> b = b a
