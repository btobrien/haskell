
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

-- finite memo table (for non-contiguous/sparse keys
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

type Stack_ a = String -> (String, a)
push_ :: Char -> Stack_ ()
push_ char stack = (char:stack,())

pop_ :: Stack_ Char
pop_ [] = undefined
pop_ stack = (tail stack, head stack)

-- oof .. ugly -- not composable
twoStepsForwardOneStepBack_ :: Stack_ Char
twoStepsForwardOneStepBack_ stack = let
    (stack1, back) = pop_ stack
    (stack2, _) = push_ 'x' stack1 
    (stack3, _) = push_ 'o' stack2 
    in
    (stack3, back)

--

-- defines a basic monadic type for threading string stack state through computations
-- note: should really be generalized for any type of stack (instead of just lists of chars
data Stack a = Stack (String -> (String,a))

-- unwraps Stack
runStack :: Stack a -> (String -> (String,a))
runStack (Stack p) = p

-- returns a parser that modifies the current state accordingly
modify :: (String -> String) -> Stack ()
modify fn = Stack $ \string -> (fn string, ())

-- returns a parser that inspects the current state and yields the value accordingly
look :: (String -> a) -> Stack a
look view = Stack $ (,) <*> view

instance Functor Stack where 
 -- fmap :: (a -> b) -> Stack a -> Stack b
    fmap f (Stack parse) = Stack (fmap f . parse)

instance Applicative Stack where 
 -- pure :: a -> Stack a
    pure x = Stack $ flip (,) x
 -- <*> :: Stack (a -> b) -> Stack a -> Stack b
    Stack nextStacker <*> Stack stacker = Stack $ \initial -> let
        (stacked, token) = stacker initial
        --(stacked', fn) = nextStacker stacked
        in
        --(stacked', fn token)
        ($token) <$> nextStacker stacked
    
instance Monad Stack where 
 -- return :: a -> Stack a
    return = pure
 -- >>= :: Stack a -> (a -> Stack b) -> Stack b
    Stack stacker >>= nextStackOf = Stack $ \initial -> let
        (stacked, token) = stacker initial
        in
        runStack (nextStackOf token) stacked
    
push :: Char -> Stack ()
push = modify . (:)

peek :: Stack Char
peek = look head

pop :: Stack Char
pop = do
    x <- peek
    modify tail
    return x

pop' :: Stack Char
pop' = 
    peek >>= \x ->
    modify tail >>= \_ ->
    return x

twoStepsForwardOneStepBack :: Stack Char
twoStepsForwardOneStepBack = do
    popped <- pop
    push 'x'
    push 'o'
    return popped

fourStepsForwardTwoStepsBack :: Stack (Char,Char)
fourStepsForwardTwoStepsBack = 
    (,) <$> twoStepsForwardOneStepBack <*> twoStepsForwardOneStepBack

example :: Stack (Char,Char,Char)
example = do
    x <- twoStepsForwardOneStepBack
    push x
    (y,z) <- fourStepsForwardTwoStepsBack
    return (x,y,z)
















