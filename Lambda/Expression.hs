
module Expression where

import Data.List
import Data.Char (isSpace)
import Control.Monad
import Text.ParserCombinators.Parsec

data Expression = Name String | Function String Expression | Application Expression Expression deriving Show

showExpr :: Expression -> String
showExpr (Name x) = x
showExpr (Function arg body) = '\\' : arg ++ " -> " ++ show body
showExpr (Application fn arg) = addParensIf isFunction fn ++ " " ++ addParensIf (not.isName) arg
    where
    addParensIf needsParens expr = if needsParens expr then '(' : show expr ++ ")" else show expr

-- define equality (a.k.a renaming)
-- define reading (from parsing)
-- implement beta/eta reductions 
-- introduce definitions
-- preprocess/import definitions

isName :: Expression -> Bool
isName (Name _) = True
isName _ = False

isFunction :: Expression -> Bool
isFunction (Function _ _) = True
isFunction _ = False

expression :: GenParser Char () Expression
expression = function <|> application <|> parenthesized expression

function :: GenParser Char () Expression
function = 
    spaces >> char '\\' >>
    word >>= \arg ->
    spaces >> string "->" >> spaces >>
    expression >>= \body ->
    return (Function arg body)

application :: GenParser Char () Expression
application = foldl1 Application <$> expressions

expressions :: GenParser Char () [Expression]
expressions = many1 $ spaces >> (parenthesized expression <|> name)

parenthesized :: GenParser Char () a -> GenParser Char () a
parenthesized parser = char '(' >> spaces >> parser >>= \x -> spaces >> char ')' >> return x

name :: GenParser Char () Expression
name = Name <$> word

word :: GenParser Char () String
word = spaces >> many1 (noneOf " ()")

idex = Function "x" (Name "x")
selfApply = Function "x" (Application (Name "x") (Name "x"))

