
module Data.Group.Permutation where

import Data.List hiding (cycle)
import Control.Applicative
import Prelude hiding (cycle)
import Data.Maybe

import Utils
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Group
import Data.Group.Action
--import Data.Hashable (hash)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

type Cycle a = [a]

showcycle :: Show a => Cycle a -> String
showcycle cycle = '(' : (concat . map show $ cycle) ++ ")"

through :: Eq a => [Cycle a] -> a -> a
through = flip (foldr rotate)
    where
    rotate cycle x = if null rotated then x else head rotated
        where
        rotated = drop 1 . dropWhile (/=x) . ((++) <$> id <*> take 1) $ cycle

permute :: Ord a => [a] -> a -> a
permute xs x = (xs!!) . length . takeWhile (<x) . sort $ xs

recycle :: Ord a => (a -> a) -> [a] -> [Cycle a]
recycle f = filter ((>1).length) . unfold popcycle . sort . nub
    where
    popcycle (x:xs) = let cycle = generate f x in (cycle, xs \\ cycle)

newtype Permutation a = P [Cycle a]; cyclesOf (P cs) = cs

toPermutation :: Ord a => [a] -> Permutation a
toPermutation = P . (flip recycle <*> permute)

reduce :: Ord a => [Cycle a] -> Permutation a
reduce = P . (recycle <$> through <*> concat)

c :: Ord a => Cycle a -> Permutation a
c = reduce . (:[])

transposition :: Ord a => (a,a) -> Permutation a
transposition (x,y) = c [x,y]

permutations :: Ord a => [a] -> [Permutation a]
permutations = map toPermutation . Data.List.permutations

instance Show a => Show (Permutation a) where
    show = coallesce . concat . map showcycle . cyclesOf where 
        coallesce [] = "()"
        coallesce s = s
instance Eq a => Eq (Permutation a) where
    (==) = (==) `on` cyclesOf
instance Ord a => Ord (Permutation a) where
    compare = compare `on` cyclesOf
instance Ord a => Semigroup (Permutation a) where
    (<>) = reduce .: (++) `on` cyclesOf
instance Ord a => Monoid (Permutation a) where
    mempty = P []
    mappend = (<>)
instance Ord a => Group (Permutation a) where
    inverse = reduce . reverse . map reverse . cyclesOf

apply :: Ord a => Permutation a -> a -> a
apply = through . cyclesOf

fromPermutation :: Ord a => Permutation a -> ([a] -> [a])
fromPermutation p = map (apply p)

shuffle :: Ord a => Permutation Int -> [a] -> [a]
shuffle p xs = sortOn' (apply p . indexIn xs) $ xs
    where
    indexIn xs x = (+1) . fromJust $ elemIndex x xs

shuffleAround :: Ord a => a -> (Permutation Int) -> [a] -> [a]
shuffleAround x pi xs = insertAt n x xs'
    where
    xs' = shuffle pi $ delete x xs
    n = fromJust $ x `elemIndex` xs

elements :: Ord a => Permutation a -> [a]
elements = sort . nub . concat . cyclesOf

mapping :: Ord a => Permutation a -> [(a,a)]
mapping = map <$> also.apply <*> elements

symmetric :: Int -> [Permutation Int]
symmetric n = Data.Group.Permutation.permutations [1..n]

transpositions :: Ord a => Permutation a -> [(a,a)]
transpositions = concat . map neighbors . cyclesOf

alternate :: Ord a => Permutation a -> Bool
alternate = Prelude.even . sum . map (length.tail) . cyclesOf

alternating :: Int -> [Permutation Int]
alternating = filter Data.Group.Permutation.alternate . symmetric

--alternates :: Ord a => Permutation a -> [Permutation a]
--alternates = map fold . chunksOf 2 . reverse . map transposition . transpositions

mash :: Eq a => Int -> [a] -> [a]
mash n = unfoldr $ \xs ->
    if null xs then Nothing
    else let xs' = rotate n xs in Just (head xs' , mash n (tail xs'))
    --else let xs' = rotate n xs in Just (head xs' , mash (hash . show $ n) (tail xs'))

selectPermutation :: Ord a => Int -> [a] -> Permutation a
selectPermutation = toPermutation .: mash

selectEvenPermutation :: Ord a => Int -> [a] -> Permutation a
selectEvenPermutation n xs = head [ a | m <- [n..], let a = selectPermutation m xs, alternate a]

impliedSet :: Ord a => [Permutation a] -> [a]
impliedSet = nub . concatMap elements

permutationOrbits :: Ord a => [Permutation a] -> [[a]]
permutationOrbits = flip orbits apply <*> impliedSet 


