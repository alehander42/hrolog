module Parser
    ( parseProlog
    ) where

import qualified Syntax as Syntax 

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Token
import Data.Functor.Identity
import Text.Parsec.Language

-- e := fact | atom
-- fact := name '(' (arg ',')* arg ')' '.'
-- atom := name
 
langDef :: LanguageDef ()
langDef = LanguageDef
  {
      commentStart    = "{#"
    , commentEnd      = "#}"
    , commentLine     = "#"
    , identStart      = letter
    , identLetter     = alphaNum <|> oneOf "_"
    , reservedNames   = ["help", "exit", "compile", "sashe"]
  }

lexer :: TokenParser ()
lexer = makeTokenParser langDef

reserved2 :: String -> Parser ()
reserved2 = reserved lexer

parens2 :: Parser a -> Parser a
parens2 = parens lexer

semiSep2 :: Parser a -> Parser [a]
semiSep2 = semiSep lexer

table ::  Ex.OperatorTable String () Identity Syntax.E
table = []

nameSymbol :: Parser Char
nameSymbol = oneOf "abcdefghijklmnopqrstuvwxyz"

name :: Parser Syntax.E
name = do
    x <- many nameSymbol
    let z = Syntax.toName x
    return z

args :: Parser [Syntax.E]
args = do
    arg <- name
    return [arg]
 
fact :: Parser Syntax.E
fact = do
    factName <- name
    char '('
    factArgs <- args
    char ')'
    char '.'
    return (Syntax.Fact factName factArgs)
   
grammar :: Parser Syntax.E
grammar = 
       fact
   <|> name
    

expr :: Parser Syntax.E
expr = Ex.buildExpressionParser table grammar

program :: Parser a -> Parser a
program p = do
    whiteSpace lexer
    r <- p
    eof
    return r

toplevel ::Parser [Syntax.E]
toplevel = semiSep2 expr 

parseProlog :: String -> Either ParseError Syntax.E
parseProlog s = parse (program expr) "<stdin>" s
    
    
