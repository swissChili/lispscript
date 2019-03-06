module Main where

import Parser
import Codegen
import Data.List
import Types
import Monads

main :: IO ()
main = do
  s <- readFile "hello.ls"
  case lispParser s of
    Right a -> genTopLevel a |> putStrLn
    Left a -> print a
