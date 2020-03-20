
import Expression
import Parse
import Text.ParserCombinators.Parsec hiding (Parser)
import Plus

infixl 9 <::>
(<::>) = (showLambda . betaReduce) .: Application `on` lambda

printx = putStrLn . showLambda

infixr 0 ||| 
name ||| x = putStr (take 13 (name ++ repeat ' ')) >> print x

idx = "\\x -> x" 
self = "\\x -> x x" 
k = "\\k -> \\_ -> k" 

reducible expr = expr /= reduce expr

-- TODO: exit on failure
main = do
    "parens"    ||| lambda "((f x))" == lambda "f x"
    "id"        ||| idx<::>idx == idx
    "lazy"      ||| k<::>"x"<::>(self<::>self) == "x"
    "apps"      ||| "f"<::>"x"<::>"y" == "f x y"
    "eta"       ||| lambda "\\x -> f x" <=> lambda "f"
    "eta"       ||| lambda "\\x y -> f x y" <=> lambda "f"
    "eta1"      ||| etaReduce (lambda "\\f -> f x") `seq` True
    "eta2"      |||  not . reducible . lambda $ "\\x -> (f x) x"
    "alpha"     ||| lambda "(\\x -> \\f -> f x) f" <=> lambda "\\f' -> f' f"
    "alpha2"    ||| lambda "(\\x -> \\f -> f x) (f' f)" <=> lambda "\\f'' -> f'' (f' f)"
    "beta"      ||| self<::>idx == idx

