
module Lambda.Parse where

import Lambda
import Control.Monad
import Text.ParserCombinators.Parsec hiding (Parser)
import Data.Either

type Parser = GenParser Char ()

instance Show Lambda.Expression where
    show (Variable x) = x
    show (Abstraction var body) = '\\' : var ++ " -> " ++ show body
    show (Application fn arg) = addParensIf isAbstraction fn ++ " " ++ addParensIf (not.isVariable) arg
        where
        addParensIf needsParens expr = if needsParens expr then '(' : show expr ++ ")" else show expr

-- TODO : better error reporting
lambda :: String -> Expression
lambda = fromRight undefined . parse expression []

evaluate :: String -> Expression
evaluate = reduce . lambda

expression :: Parser Expression
expression = abstraction <|> application <|> parenthesized expression

abstraction :: Parser Expression
abstraction = 
    abstractionVariables >>= \vars ->
    expression >>= \body ->
    return (foldr Abstraction body vars)

abstractionVariables :: Parser [String]
abstractionVariables =
    spaces >> char '\\' >>
    many1 word >>= \vars ->
    match "->" >>
    return vars

application :: Parser Expression
application = foldl1 Application <$> expressions

expressions :: Parser [Expression]
expressions = many1 $ spaces >> (parenthesized expression <|> variable)

parenthesized :: Parser a -> Parser a
parenthesized parser = spaces >> char '(' >> spaces >> parser >>= \x -> spaces >> char ')' >> spaces >> return x

variable :: Parser Expression
variable = Variable <$> word

-- why try?
word :: Parser String
word = try $ spaces >> many1 (noneOf (" ()-\\"++['1'..'9'])) >>= \x -> spaces >> return x

match :: String -> Parser ()
match str = spaces >> string str >> spaces
