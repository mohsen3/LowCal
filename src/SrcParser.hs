module SrcParser where

import SrcTypes

import Control.Monad (void, when, unless, guard)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

keywords = ["def", "do", "end", "if", "then", "else", "let"]

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser ()
keyword kw = void $ sc *> string kw <* sc

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

upperCaseName :: String -> Parser String
upperCaseName label = do
    first <- upperChar <?> label
    rest <- many alphaNumChar
    let name = first:rest
    if name `elem` keywords
    then
      fail "Keyword found"
    else
      return name

moduleParser :: Parser SrcModule
moduleParser = fmap SrcModule (many functionParser <* eof)

functionParser :: Parser SrcFunctionDef 
functionParser = do
  keyword "def"
  name <- lowerCaseName "functionName"
  args <- between (symbol "(") (symbol ")") (lowerCaseName "functionArg" `sepBy` symbol ",")
  expressions <- between (keyword "do") (keyword "end") (some $ try expressionParser)
  return $ SrcFunctionDef name args expressions

expressionParser :: Parser SrcExp
expressionParser = choice [letParser, ifParser, primitiveParser, listParser, try functionCallParser, varParser] <?> "expression"
  where
      listParser = do
          items <- between (symbol "[") (symbol "]") (expressionParser `sepBy` symbol ",")
          return $ SrcExpList items
      primitiveParser = fmap SrcExpPrimitive valueParser
      varParser = fmap SrcExpVar (lowerCaseName "variableName")
      letParser = do
          keyword "let"
          name <- lowerCaseName "variableName"
          space
          keyword "="
          value <- expressionParser
          return $ SrcLetExp name value
      ifParser = do
          keyword "if"
          condition <- expressionParser
          keyword "then"
          thenBlock <- some $ try expressionParser
          keyword "else"
          elseBlock <- some $ try expressionParser
          keyword "end"
          return $ SrcIfExp condition thenBlock elseBlock
      functionCallParser = do
          moduleName <- optional (upperCaseName "ModuleName" <* symbol ".")
          functionName <- lowerCaseName "functionName"
          params <- between (symbol "(") (symbol ")") (expressionParser `sepBy` symbol ",")
          return $ SrcFunctionCall moduleName functionName params


valueParser :: Parser SrcPrimitive
valueParser = choice [holeParser, intParser]
  where
    holeParser = do
        symbol "_"
        return SrcHole
    intParser = do
        num <- lexeme L.decimal
        return $ SrcIntValue num
