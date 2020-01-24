
module Data.Matrix where

import Control.Applicative ((<$>),(<*>))

import Data.List (transpose, nub)
import Utils (chunksOf, (.:))

type Matrix a = [[a]]

(.:.) :: Num a => [a] -> [a] -> a
(.:.) = sum .: zipWith (*)

(.+.) :: Num a => [a] -> [a] -> [a]
(.+.) = zipWith (+)

(<.:.>) :: Num a => Matrix a -> Matrix a -> [a]
x <.:.> y = (.:.) <$> x <*> y

rows :: Matrix a -> Int
rows = length

cols :: Matrix a -> Int
cols = length . head

(.*.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.*.) x y | (cols x == rows y) =
    chunksOf (cols y) (x <.:.> transpose y)

(*.) :: Num a => Matrix a -> [a] -> [a]
(*.) m x | (cols m == length x) = (.:.x) <$> m 

(<|>) :: Matrix a -> Matrix a -> Matrix a
x <|> y | rows x == rows y = zipWith (++) x y

(<->) :: Matrix a -> Matrix a -> Matrix a
x <-> y | cols x == cols y = x ++ y

identity :: Num a => Int -> Matrix a
identity 1 = [[1]]
identity n | n > 1 = let zeros = replicate (n-1) 0
    in
                    [1:zeros]
                       <->
    (transpose [zeros] <|> identity (n-1))

space :: (Num a, Enum a, Bounded a) => Int -> [[a]]
space n = sequence $ replicate n [minBound..maxBound]

kernel :: (Eq a, Num a, Enum a, Bounded a) => Matrix a -> [[a]]
kernel m = [ x | x <- space (cols m), all (==0) (m *. x)]

image :: (Eq a, Num a, Enum a, Bounded a) => Matrix a -> [[a]]
image m = nub $ (m *.) <$> space (cols m)

