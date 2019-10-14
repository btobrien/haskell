
import Data.List

divisibleBy :: Int -> Int -> Bool
divisibleBy x = (==0) . (`mod`x)

divides :: Int -> Int -> Bool
divides = flip divisibleBy

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = (x:) . sieve . filter (not . divisibleBy x) $ xs
-- interesting performance difference with sieve before filtering....
-- manages to improve perf above foldr (?)

siever :: [Int] -> [Int]
siever = foldr (\x -> (x:) . filter (not . divisibleBy x)) []
 
-- reminder: left folds can't handle infinite lists
sievel :: [Int] -> [Int]
sievel = reverse . foldl (\xs x -> if any (divides x) xs then xs else x:xs) []
 
sievel' :: [Int] -> [Int]
sievel' = reverse . foldl' (\xs x -> if any (divides x) xs then xs else x:xs) []

primes :: [Int]
primes = sieve [2..]

primesUnder :: Int -> [Int]
primesUnder n = sieve [2..(n-1)]

largestPrimeUnder :: Int -> Int
largestPrimeUnder = last . primesUnder

-- sorted smallest -> largest
denominators :: Int -> [Int]
denominators n = filter (divides n) [2..half] ++ [n]
    where half = n `div` 2 

smallestDenominator :: Int -> Int
smallestDenominator = head . denominators

factor :: Int -> [Int]
factor = unfoldr (\n -> if n == 1 then Nothing else let d = smallestDenominator n in Just (d, n `div` d))

isPrime :: Int -> Bool
isPrime n = factor n == [n]

-- slow
-- primeFilter = filter isPrime [2..]
