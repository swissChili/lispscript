module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Types

whitespaces = many1 $ oneOf " \n\t\r"
whitespacesOpt = many $ oneOf " \n\t\r"

ident = do
  i <- many1 $ letter 
           <|> oneOf "$_."
  return $ Identifier i

remainingSpacedIdents :: GenParser Char st [Lisp]
optionalSpaceSepIdents = option [] spaceSepIdents

spaceSepIdents = do
  first <- ident
  next <- remainingSpacedIdents
  return ( first : next )


remainingSpacedIdents = many $ whitespaces >> ident

sizeableFn a pa b = do
  char '('
  whitespacesOpt
  string a
  whitespaces
  vals <- many1 $ pa
  whitespacesOpt
  char ')'
  return $ b vals

binaryFn a pa pb b = do
  char '('
  whitespacesOpt
  string a
  whitespaces
  parta <- pa
  whitespaces
  partb <- pb
  whitespacesOpt
  char ')'
  return $ b parta partb

letFn = binaryFn "let" ident topLevelP Binding
setFn = binaryFn "set" ident topLevelP Assignment

opFn a = binaryFn a topLevelP topLevelP $ Operator a

addFn = opFn"+"
subFn = opFn "-"
multFn = opFn "*"
divFn = opFn "/"

doBlock = sizeableFn "do" topLevelP DoBlock

invocation = do
  char '('
  whitespacesOpt
  func <- topLevelP
  args <- many $ whitespaces >> topLevelP
  whitespacesOpt
  char ')'
  return $ Invocation func args

strLit = do
  char '"'
  content <- many $ noneOf ['"', '\n']
  char '"'
  return $ StringLit content

intLit = do
  d <- toi <$> many1 digit
  return $ IntLit d
  where toi = read :: String -> Int

-- [a b]: { (+ a b) }
-- equivelant of a `do` block in most fp languages
-- but with the added benefit of args
lambda = do
  whitespacesOpt
  char '['
  args <- optionalSpaceSepIdents
  char ']'
  whitespacesOpt
  string "=>"
  whitespacesOpt
  body <- topLevelP
  return $ Lambda args body

end = do
  many1 $ oneOf " \t\n\r"
  return Ignore

comment = do
  char ';'
  many $ noneOf "\n"
  char '\n'
  return Ignore


topLevelP = try comment
        <|> try letFn
        <|> try setFn
        <|> try addFn
        <|> try subFn
        <|> try multFn
        <|> try divFn
        <|> try invocation
        <|> try lambda
        <|> try doBlock
        <|> try intLit
        <|> try strLit
        <|> try ident
        <|> try end

top = many1 topLevelP

lispParser = parse top "(unknown)"
