module SrcParser where

import SrcTypes

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

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
lowerCaseName name = do
    first <- lowerChar <?> name
    rest <- many alphaNumChar
    return (first:rest)

functionParser :: Parser SrcFunctionDef 
functionParser = do
  keyword "def"
  name <- lowerCaseName "functionName"
  args <- between (symbol "(") (symbol ")") (lowerCaseName "functionArg" `sepBy` symbol ",")
  keyword "do"
  expressions <- some expressionParser
  keyword "end"
  -- ...
  return $ SrcFunctionDef name args expressions

expressionParser :: Parser SrcExp
expressionParser = choice [primitiveParser, listParser] <?> "expression"
  where
      listParser = do
          items <- between (symbol "[") (symbol "]") (expressionParser `sepBy` symbol ",")
          return $ SrcExpList items

      primitiveParser = do
          val <- valueParser
          return $ SrcExpPrimitive val

valueParser :: Parser SrcPrimitive
valueParser = choice [holeParser, intParser]
  where
    holeParser = do
        symbol "_"
        return SrcHole
    intParser = do
        num <- lexeme L.decimal
        return $ SrcIntValue num
