{-# OPTIONS_GHC -fobject-code -O2 #-}

import Data.Maybe
import Data.List (delete)

import qualified Data.Array.Unboxed as U
import qualified Data.Array as Array
import Data.Array ((!), array, Array, Ix, range, bounds, inRange)
import Data.Ix



-- Find the ways that a given integer, X , can be expressed as the sum of the Nth power of unique, natural numbers.
powerSums :: Int -> Int -> Int -> [[Int]]
powerSums _ _ x | x < 0 = []
powerSums _ _ x | x == 0 = [[]]
powerSums prev n x = flip concatMap [prev + 1 .. root n x] $ \next -> map (next:) (powerSums next n (x - next^n))
    where 
    root n x = ceiling $ fromIntegral x ** (1 / fromIntegral n)



polygonics :: Int -> [Int]
polygonics d = scanl1 (+) . map (\x -> (d-2) * x - (d-3)) $ [1..]
-- add the new edges besides the 2 that are shared and subtract the all but 3 vertices that would be double counted
-- note: how both the familiar square and linear(!) numbers are also contained in the pattern :)

polygonic :: Int -> (Int -> Int) -> Int -> Int
polygonic _ _ 0 = 0
polygonic d recurse n = ((d-2) * n - (d-3)) + recurse (n-1)
-- add the new edges besides the 2 that are shared and subtract the all but 3 vertices that would be double counted

-- finite memo table
memoized :: Ix a => ((a -> b) -> a -> b) -> (a,a) -> Array a b
memoized fn bounds = array bounds $ (,)<*> fn (memoized fn bounds !) <$> range bounds

memo = (memoized (polygonic 5) (0,100000) !)

--foldseq :: Ix a => a -> Array a b -> b
foldseq :: Int -> Array Int b -> Array Int b
foldseq i xs = if inRange (bounds xs) i then xs!i `seq` foldseq (i+1) xs else xs

unbox :: Ix a => Array a b -> U.Array a b
unbox xs = U.array (bounds xs) (assocs xs)
