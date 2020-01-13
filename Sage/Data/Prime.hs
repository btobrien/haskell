
module Data.Prime where

import Data.List
import Utils

divisibleBy :: Int -> Int -> Bool
divisibleBy x = (==0) . (`mod`x)

divides :: Int -> Int -> Bool
divides = flip divisibleBy

(|.) :: Int -> Int -> Bool
(|.) = divisibleBy

divisors :: Int -> [Int]
divisors n = filter (divides n) [2..(n `div` 2)]

divisors' :: Int -> [Int]
divisors' n = 1 : divisors n ++ [n]

smallestDivisor :: Int -> Int
smallestDivisor = head . tail . divisors'

largestDivisor :: Int -> Int
largestDivisor = last . divisors

factor :: Int -> [Int]
factor = unfoldr $ \n ->
    if n == 1 then Nothing
    else let d = smallestDivisor n in Just (d, n `div` d)

isPrime :: Int -> Bool
isPrime n = factor n == [n]

factors :: Int -> [(Int,Int)]
factors = map ((\(xs,n) -> (head xs, n)) . also length) . group . factor

-- returns the linear combination which produces their gcd
euclidean :: Int -> Int -> (Int,Int)
euclidean a b = euclidean' (a,b) (1,0) (0,1)
    where
    euclidean' base a b = let
        r = a `plus` scale (-q) b
        q = dot base a `div` dot base b
		in
        if dot base b == 0 then a
        else euclidean' base b r
        where
        dot (a,b) (r,s) = (a*r) + (b*s)
        plus (a,b) (r,s) = (a+r,b+s)
        scale k (a,b) = (k*a,k*b)

totient :: Int -> Int
totient n = let ps = nub (factor n) in product (map (subtract 1) ps) * (n `div` product ps)

primes :: [Int]
primes = sieve [2..]
    where
    sieve [] = []
    sieve (x:xs) = (x:) . sieve . filter (not . divisibleBy x) $ xs

