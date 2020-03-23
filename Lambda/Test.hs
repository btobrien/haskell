
import Lambda
import Ski
import Lambda.Parse
import Text.ParserCombinators.Parsec hiding (Parser)
import Plus

infixl 9 <::>
(<::>) = (betaReduce) .: Lambda.Application `on` lambda

infixr 0 ||| 
name ||| x = putStr (take 13 (name ++ repeat ' ')) >> print x

idx = "\\x -> x" 
self = "\\x -> x x" 
constant = "\\x -> \\_ -> x" 

-- TODO: exit on failure
main = do
    "parens"        ||| lambda "((f x))" == lambda "f x"
    "id"            ||| idx<::>idx == idx
    "lazy"          ||| constant<::>"x"<::>(self<::>self) == "x"
    "apps"          ||| "f"<::>"x"<::>"y" == "f x y"
    "eta"           ||| lambda "\\x -> f x" <=> lambda "f"
    "eta"           ||| lambda "\\x y -> f x y" <=> lambda "f"
    "eta1"          ||| etaReduce (lambda "\\f -> f x") `seq` True
    "eta2"          |||  not . reducible . lambda $ "\\x -> (f x) x"
    "shadow"        ||| lambda "(\\x -> \\f -> f x) f" <=> lambda "\\f' -> f' f"
    "shadow2"       ||| lambda "(\\x -> \\f -> f x) (f' f)" <=> lambda "\\f'' -> f'' (f' f)"
    "alphaNormal"   ||| lambda "\\x y -> y x" <=> lambda "\\f g -> g f"
    "beta"          ||| self<::>idx == idx

