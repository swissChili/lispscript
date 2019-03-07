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

-- Praise be verbosity
optionalspaceSep t = option [] $ spaceSep t

spaceSep t = do
  first <- t
  next <- remainingSpaceSep t
  return ( first : next )


remainingSpaceSep t = many $ whitespaces >> t

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

method = do
  char '('
  whitespacesOpt
  char '.'
  call <- ident
  whitespaces
  object <- topLevelP
  args <- many $ whitespaces >> topLevelP
  whitespacesOpt
  char ')'
  return $ Method object call args

strLit = do
  char '"'
  content <- many $ noneOf ['"', '\n']
  char '"'
  return $ StringLit content

singleQuotedLit = do
  char '\''
  content <- many $ noneOf "'\n"
  char '\''
  return $ StringLit content

intLit = do
  d <- toi <$> many1 digit
  return $ IntLit d
  where toi = read :: String -> Int

lambda = do
  whitespacesOpt
  char '['
  args <- optionalspaceSep ident
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

array = do
  whitespacesOpt
  char '['
  whitespacesOpt
  items <- optionalspaceSep topLevelP
  whitespacesOpt
  char ']'
  return $ Array items

topLevelP = try comment
        <|> try letFn
        <|> try setFn
        <|> try addFn
        <|> try subFn
        <|> try multFn
        <|> try divFn
        <|> try doBlock
        <|> try method
        <|> try invocation
        <|> try lambda
        <|> try array
        <|> try intLit
        <|> try strLit
        <|> try singleQuotedLit
        <|> try ident
        <|> try end

top = many1 topLevelP

lispParser = parse top "(unknown)"
