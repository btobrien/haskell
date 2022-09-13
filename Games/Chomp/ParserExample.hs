
import Plus 

import Data.Maybe
import Data.List (delete)

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!), Map)

--
-- Monads...


-- defines a basic monadic type for threading string state through computations
-- note: non-basic parser would really encode some notion of possible failure in the computation
data Parser a = Parser (String -> Maybe (String,a))

-- unwraps Parser
parseWith :: Parser a -> (String -> Maybe (String,a))
parseWith (Parser p) = p

-- returns a parser that modifies the current state accordingly
modify :: (String -> String) -> Parser ()
modify fn = Parser $ \string -> (fn string, ())

-- returns a parser that inspects the current state and yields the value accordingly
look :: (String -> a) -> Parser a
look view = Parser $ (,) <*> view

instance Functor Parser where 
 -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser parse) = Parser (fmap f . parse)

instance Applicative Parser where 
 -- pure :: a -> Parser a
    pure x = Parser $ flip (,) x
 -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    Parser nextParse <*> Parser parse = Parser $ \string -> let
        (parsed, token) = parse string
        --(parsed', fn) = nextParse parsed
        in
        --(parsed', fn token)
        ($token) <$> nextParse parsed
    
instance Monad Parser where 
 -- return :: a -> Parser a
    return = pure
 -- >>= :: Parser a -> (a -> Parser b) -> Parser b
    Parser parse >>= nextParser = Parser $ \string -> let
        (parsed, token) = parse string
        in
        parseWith (nextParser token) parsed
    
push :: Char -> Parser ()
push = modify . (:)

peek :: Parser Char
peek = look head

pop :: Parser Char
pop = do
    x <- peek
    modify tail
    return x

pop' :: Parser Char
pop' = 
    peek >>= \x ->
    modify tail >>= \_ ->
    return x

twoStepsForwardOneStepBack :: Parser Char
twoStepsForwardOneStepBack = do
    popped <- pop
    push 'x'
    push 'o'
    return popped

fourStepsForwardTwoStepsBack :: Parser (Char,Char)
fourStepsForwardTwoStepsBack = 
    (,) <$> twoStepsForwardOneStepBack <*> twoStepsForwardOneStepBack

example :: Parser (Char,Char,Char)
example = do
    x <- twoStepsForwardOneStepBack
    push x
    (y,z) <- fourStepsForwardTwoStepsBack
    return (x,y,z)
















