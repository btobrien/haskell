
{-# OPTIONS_GHC -fobject-code -O2 #-}

import Plus 

import Data.Maybe
import Data.List (delete)

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!), Map)

-- no memoization
fibNaive :: Int -> Int
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n-1) + fibNaive (n-2)

-- uses open recursion in order to be memoized
fib :: (Int -> Int) -> Int -> Int
fib _ 0 = 0
fib _ 1 = 1
fib fib n = fib (n-1) + fib (n-2)

-- self memoized
fibs :: [Int]
fibs = fib (fibs !!) <$> [0..]
    where
    fib _ 0 = 0
    fib _ 1 = 1
    fib fib' n = fib' (n-1) + fib' (n-2)

-- finite memo table
memoized :: Ord a => ((a -> b) -> a -> b) -> [a] -> Map a b
memoized fn xs = Map.fromList $ (,)<*> fn (memoized fn xs !) <$> xs

memoize :: Ord a => (a -> [a]) -> ((a -> b) -> a -> b) -> a -> b
memoize subdomain fn = (!) . memoized fn . subdomain <*> id

-- infinite memo table (linear lookup is brutal, switch to inf trie)
memoized' :: Eq a => ((a -> b) -> a -> b) -> [a] -> [(a,b)]
memoized' fn xs = (,)<*> fn (fromJust . flip lookup (memoized' fn xs)) <$> xs

memoize' :: Eq a => [a] -> ((a -> b) -> a -> b) -> a -> b
memoize' xs fn = fromJust . flip lookup (memoized' fn xs)

-- infinite memo list (where index is the implied argument)
memoized'' :: ((Int -> b) -> Int -> b) -> [b]
memoized'' fn = fn (memoized'' fn !!) <$> [0..]

memoize'' :: ((Int -> b) -> Int -> b) -> Int -> b
memoize'' fn = (memoized'' fn !!)

--
-- Monads...

type Parser_ a = String -> (String, a)
push_ :: Char -> Parser_ ()
push_ char stack = (char:stack,())

pop_ :: Parser_ Char
pop_ [] = undefined
pop_ stack = (tail stack, head stack)

-- oof .. ugly -- not composable
twoStepsForwardOneStepBack_ :: Parser_ Char
twoStepsForwardOneStepBack_ stack = let
    (stack1, back) = pop_ stack
    (stack2, _) = push_ 'x' stack1 
    (stack3, _) = push_ 'o' stack2 
    in
    (stack3, back)

--

-- defines a basic monadic type for threading string state through computations
data Parser a = Parser (String -> (String,a))

-- unwraps Parser
parseWith :: Parser a -> (String -> (String,a))
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
















