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
  where s = c ++ "(\"" ++ a ++ "\")"

genRec c ((IntLit a):xs) = genRec s xs
  where s = c ++ show a

genRec c ((Identifier a):xs) = genRec s xs
  where s = c ++ "(" ++ a ++ ")"

genRec c ((Invocation f a):xs) = genRec s xs
  where
    s = c ++ "(" ++ genJs [f] ++ "(" ++ args ++ "))"
    args = intercalate "," $ a |> map \x -> genJs [x]

genRec c ((Constructor o a):xs) = genRec s xs
  where
    s = c ++ "(new " ++ genJs [o] ++ "(" ++ args ++ "))"
    args = intercalate "," $ genJs . pure <$> a

genRec c ((Method obj call a):xs) = genRec s xs
  where
    s = c ++ genJs [obj] ++ "." ++ unwrappedIdent call ++ "(" ++ args ++ ")"
    args = intercalate "," $ a |> map \x -> genJs [x]

genRec c ((Lambda a b):xs) = genRec s xs
  where
    s = "((" ++ csargs ++ ")=>" ++ body ++ ")"
    csargs = intercalate "," $ unwrappedIdent <$> a
    body = genJs [b]

genRec c ((Binding k v):xs) = genRec s xs
  where
    s = c ++ "var " ++ unwrappedIdent k ++ "=" ++ genJs [v]

genRec c ((Assignment k v):xs) = genRec s xs
  where
    s = c ++ unwrappedIdent k ++ "=" ++ genJs [v]

genRec c ((Operator o a):xs) = genRec s xs
  where
    s = c ++ "(" ++ items ++ ")"
    items = intercalate o $ genJs . pure <$> filter notIgnore a

genRec c (Ignore:xs) = genRec c xs

genRec c ((Array a):xs) = genRec s xs
  where
    s = c ++ "[" ++ csitems ++ "]"
    csitems = intercalate "," $ genJs . pure <$> a

genRec c (End:xs) = genRec s xs
  where s = c ++ ";\n"

genRec c ((IfStatement clause a b):xs) = genRec s xs
  where
    s = c ++ "(" ++ genJs [clause] ++ ")?" ++ genJs [a] ++ " : " ++ genJs [b]

genRec c ((KeyValPair key val):xs) = genRec s xs
  where
    s = c ++ unwrappedIdent key ++ ":" ++ genJs [val]

genRec c ((Object items):xs) = genRec s xs
  where
    s = c ++ "{" ++ cs ++ "}"
    cs = intercalate ", " $ genJs . pure <$> items

genRec c ((ObjIndex k obj):xs) = genRec s xs
  where
    s = c ++ genJs [obj] ++ "." ++ unwrappedIdent k

genRec c ((ObjIndexVar k obj):xs) = genRec s xs
  where
    s = c ++ genJs [obj] ++ "[" ++ genJs [k] ++ "]"

genRec c ((DoBlock b):xs) = genRec s xs
  where
    s = c ++ "{" ++ genTopLevel b ++ "}"

genRec c ((Return r):xs) = genRec s xs
  where s = c ++ "return " ++ genJs [r] ++ ";"

genJs = genRec ""

notIgnore Ignore = False
notIgnore _ = True

genTopLevel a = genJs $ intersperse End $ filter notIgnore a
