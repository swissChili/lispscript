module Main where

import Parser
import Codegen
import Data.List
import Types
import Monads
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> putStrLn "No program provided. Provide one as an argument"
    _ -> do
      s <- readFile $ args !! 0
      case lispParser s of
        Right a -> genTopLevel a |> putStrLn
        Left a -> print a
