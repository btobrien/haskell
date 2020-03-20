
module Parse where

import Expression
import Control.Monad
import Text.ParserCombinators.Parsec hiding (Parser)
import Data.Either

type Parser = GenParser Char ()

isVariable :: Expression -> Bool
isVariable (Variable _) = True; isVariable _ = False

isAbstraction :: Expression -> Bool
isAbstraction (Abstraction _ _) = True; isAbstraction _ = False

showLambda :: Expression -> String
showLambda (Variable x) = x
showLambda (Abstraction var body) = '\\' : var ++ " -> " ++ showLambda body
showLambda (Application fn arg) = addParensIf isAbstraction fn ++ " " ++ addParensIf (not.isVariable) arg
    where
    addParensIf needsParens expr = if needsParens expr then '(' : showLambda expr ++ ")" else showLambda expr

-- TODO : better error reporting
lambda :: String -> Expression
lambda = fromRight undefined . parse expression []

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
parenthesized parser = char '(' >> spaces >> parser >>= \x -> spaces >> char ')' >> return x

variable :: Parser Expression
variable = Variable <$> word

-- why try?
word :: Parser String
word = try $ spaces >> many1 (noneOf " ()-\\")

match :: String -> Parser ()
match str = spaces >> string str >> spaces
