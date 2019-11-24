
module Data.Group.Permutation where

import Data.List hiding (cycle)
import Control.Applicative
import Prelude hiding (cycle)

import Utils
import Data.Group
import Data.Group.Modulo

type Cycle a = [a]
-- inverse is reverse

showcycle :: Show a => Cycle a -> String
showcycle cycle = '(' : (concat . map show $ cycle) ++ ")"

through :: Eq a => [Cycle a] -> a -> a
through = flip (foldr rotate)
    where
    rotate cycle x = if null rotated then x else head rotated
        where rotated = drop 1 . dropWhile (/=x) . ((++) <$> id <*> take 1) $ cycle

-- unique letters
permute :: Ord a => [a] -> a -> a
permute xs x = (xs!!) . length . takeWhile (<x) . sort $ xs

recycle :: Ord a => (a -> a) -> [a] -> [Cycle a]
recycle f = filter ((>1).length) . unfold popcycle . sort . nub
    where
    popcycle (x:xs) = let cycle = generate f x in (cycle, xs \\ cycle)

-- reduction invariant
newtype Permutation a = P [Cycle a]; cycles (P cs) = cs

reduce :: Ord a => [Cycle a] -> Permutation a
reduce = P . (recycle <$> through <*> concat)

toPermutation :: Ord a => [a] -> Permutation a
toPermutation = P . (recycle <$> permute <*> id)

permutations :: Ord a => [a] -> [Permutation a]
permutations = map toPermutation . Data.List.permutations

instance Show a => Show (Permutation a) where
    show = coallesce . concat . map showcycle . cycles where 
        coallesce [] = "()"
        coallesce s = s
instance Eq a => Eq (Permutation a) where
    (==) = (==) `on` cycles
instance Ord a => Ord (Permutation a) where
    compare = compare `on` cycles
instance Ord a => Semigroup (Permutation a) where
    (<>) = reduce .: (++) `on` cycles
instance Ord a => Monoid (Permutation a) where
    mempty = P []
instance Ord a => Group (Permutation a) where
    inv = reduce . reverse . map reverse . cycles

apply :: Ord a => Permutation a -> a -> a
apply = through . cycles

elements :: Ord a => Permutation a -> [a]
elements = sort . nub . concat . cycles

mapping :: Ord a => Permutation a -> [(a,a)]
mapping = map <$> also.apply <*> elements

infixr 6 <:
(<:) :: Ord a => Cycle a -> Permutation a -> Permutation a
c <: p = reduce [c] <> p

symmetric :: Int -> [Permutation Int]
symmetric n = Data.Group.Permutation.permutations [1..n]

transpositions :: Ord a => Permutation a -> [(a,a)]
transpositions = concat . map neighbors . cycles

even :: Ord a => Permutation a -> Bool
even = Prelude.even . sum . map (length.tail) . cycles

alternating :: Int -> [Permutation Int]
alternating = filter Data.Group.Permutation.even . symmetric

--generalize Int to Enum

rotate :: Int -> Permutation Int
rotate n = [1..n] <:e

reflect :: Int -> Permutation Int
reflect n = reduce
    . map (\x -> [evaluate x, (evaluate.inv) x])
    . take (n`div`2) . tail
    $ modulo n

dihedral :: Int -> [Permutation Int]
dihedral n = genFrom [rotate n, reflect n]
