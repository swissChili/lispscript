{-# LANGUAGE BlockArguments #-}

module Codegen where

import Types
-- Intercalate && Intersperse are life savers
import Data.List
import Monads

unwrappedIdent (Identifier a) = a
unwrappedIdent _ = ""

genRec :: String -> [Lisp] -> String
genRec c [] = c
genRec c ((StringLit a):xs) = genRec s xs
  where s = c ++ "\"" ++ a ++ "\""

genRec c ((IntLit a):xs) = genRec s xs
  where s = c ++ show a

genRec c ((Identifier a):xs) = genRec s xs
  where s = c ++ "(" ++ a ++ ")"

genRec c ((Invocation f a):xs) = genRec s xs
  where
    s = c ++ (genJs [f]) ++ "(" ++ args ++ ")"
    args = intercalate "," $ (\x -> genJs [x]) <$> a

genRec c ((Lambda a b):xs) = genRec s xs
  where
    s = "(function(" ++ csargs ++ "){" ++ body ++ "})"
    csargs = intercalate "," $ unwrappedIdent <$> a
    body = genTopLevel b

genRec c ((Binding k v):xs) = genRec s xs
  where
    s = c ++ "var " ++ unwrappedIdent k ++ "=" ++ genJs [v]

genRec c ((Assignment k v):xs) = genRec s xs
  where
    s = c ++ unwrappedIdent k ++ "=" ++ genJs [v]

genRec c ((Operator o a b):xs) = genRec s xs
  where
    s = c ++ "(" ++ genJs [a] ++ o ++ genJs [b] ++ ")"

genRec c (Ignore:xs) = genRec c xs

genRec c (End:xs) = genRec s xs
  where s = c ++ ";\n"

genJs = genRec ""

notIgnore Ignore = False
notIgnore _ = True

genTopLevel a = genJs $ intersperse End $ filter notIgnore a
