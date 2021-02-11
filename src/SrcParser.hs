module SrcParser where

import SrcTypes

import Control.Monad (void, when, unless, guard)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

keywords = ["def", "do", "end"]

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser ()
keyword kw = void $ string kw <* space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lowerCaseName :: String -> Parser String
lowerCaseName label = do
    first <- lowerChar <?> label
    rest <- many alphaNumChar
    let name = first:rest
    if name `elem` keywords
    then
      fail "keyword found"
    else
      return name

functionParser :: Parser SrcFunctionDef 
functionParser = do
  keyword "def"
  name <- lowerCaseName "functionName"
  args <- between (symbol "(") (symbol ")") (lowerCaseName "functionArg" `sepBy` symbol ",")
  expressions <- between (keyword "do") (keyword "end") (some $ try expressionParser)
  -- ...
  return $ SrcFunctionDef name args expressions

expressionParser :: Parser SrcExp
expressionParser = choice [primitiveParser, listParser, varParser] <?> "expression"
  where
      listParser = do
          items <- between (symbol "[") (symbol "]") (expressionParser `sepBy` symbol ",")
          return $ SrcExpList items

      primitiveParser = do
          val <- valueParser
          return $ SrcExpPrimitive val

      varParser = do
          name <- lowerCaseName "variableName"
          return $ SrcExpVar name

valueParser :: Parser SrcPrimitive
valueParser = choice [holeParser, intParser]
  where
    holeParser = do
        symbol "_"
        return SrcHole
    intParser = do
        num <- lexeme L.decimal
        return $ SrcIntValue num
