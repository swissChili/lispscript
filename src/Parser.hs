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

invocation = do
  char '('
  func <- topLevelP
  args <- many $ whitespaces >> topLevelP
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
  char ':'
  whitespacesOpt
  char '{'
  body <- top
  char '}'
  return $ Lambda args body

end = do
  many1 $ oneOf " \t\n\r"
  return Ignore

topLevelP = try invocation
        <|> try lambda
        <|> try intLit
        <|> try strLit
        <|> try ident
        <|> try end

top = many1 topLevelP

lispParser = parse top "(unknown)"
