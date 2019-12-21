
module Data.Graph.Trail where

import Data.Group
import Data.List
import Control.Applicative
import Data.Group
import Utils
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Maybe
--import Data.Group.Permutation

-- assumed to be symmetric
type Graph a = a -> [a]

--run :: Ord a => [a] -> Permutation a
--run = fold . map transposition . neighbors . drop 1

isClosedTrail :: Eq a => Graph a -> [a] -> Bool
isClosedTrail next = all (\(x,y) -> x `elem` next y) . neighbors . wrap

closedTrails :: Ord a => Int -> a -> Graph a -> [[a]]
closedTrails n start graph = trails n graph start start

-- TODO: don't backtrack
trails :: Ord a => Int -> Graph a -> a -> a -> [[a]]
trails 0 _ target start = if start == target then [[]] else []
trails n graph target start = concat $ map (map (start:) . trails (n - 1) graph target) (graph start)

--solve :: Ord a => Graph a -> a -> Permutation a -> Maybe [a]
--solve graph start x = listToMaybe $ do
    --n <- [0,2..] -- pull higher
    --filter (inverts x . run) $ closedTrails n start graph

tileGraph :: (Int,Int) -> Graph (Int,Int)
tileGraph size = clean size . gen
    where
    gen (x,y) = [
        (x+1,y),
        (x-1,y),
        (x,y+1),
        (x,y-1)]
    clean size = filter (bothWithin size)
        where 
        within n x = 0 <= x && x < n
        bothWithin (n,m) (x,y) = within n x && within m y


