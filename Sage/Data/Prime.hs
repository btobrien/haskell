
module Data.Prime where

import Data.List
import Utils

divisibleBy :: Integer -> Integer -> Bool
divisibleBy x = (==0) . (`mod`x)

divides :: Integer -> Integer -> Bool
divides = flip divisibleBy

(|.) :: Integer -> Integer -> Bool
(|.) = divisibleBy

divisors :: Integer -> [Integer]
divisors n = filter (divides n) [2..(n `div` 2)]

divisors' :: Integer -> [Integer]
divisors' n = 1 : divisors n ++ [n]

smallestDivisor :: Integer -> Integer
smallestDivisor = head . tail . divisors'

largestDivisor :: Integer -> Integer
largestDivisor = last . divisors

factor :: Integer -> [Integer]
factor = unfoldr $ \n ->
    if n == 1 then Nothing
    else let d = smallestDivisor n in Just (d, n `div` d)

isPrime :: Integer -> Bool
isPrime n = factor n == [n]

--factors :: Integer -> [(Integer,Integer)]
factors = map ((\(xs,n) -> (head xs, n)) . also length) . group . factor

isPowerOf n x =
    if x == 1 then True
    else if x `mod` n /= 0 then False
    else isPowerOf n (x `div` n)

-- returns the linear combination which produces their gcd
euclidean :: Integer -> Integer -> (Integer,Integer)
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

totient :: Integer -> Integer
totient n = let ps = nub (factor n) in product (map (subtract 1) ps) * (n `div` product ps)

primes :: [Integer]
primes = sieve [2..]
    where
    sieve [] = []
    sieve (x:xs) = (x:) . sieve . filter (not . divisibleBy x) $ xs

prime :: Int -> Integer
prime = (primes!!)

hofs :: [Integer]
hofs = 1 : from 1 [1] 1 where
    from x banned n =
        if [n] == take 1 banned
        then from x (drop 1 banned) (n+1)
        else let x' = (x+n) in x' : from x' (banned++[x']) (n+1)
