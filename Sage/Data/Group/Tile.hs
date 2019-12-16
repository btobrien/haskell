
module Data.Group.Tile where

import Data.Group
import Data.List
import Control.Applicative
import Data.Group
import Utils
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Maybe
import Data.Group.Permutation

type Tile = (Int,Int)
-- assumed to be symmetric
type Graph a = a -> [a]

run :: Ord a => [a] -> Permutation a
run = fold . map transposition . neighbors . drop 1

isClosedTrail :: Eq a => Graph a -> [a] -> Bool
isClosedTrail next = all (\(x,y) -> x `elem` next y) . neighbors . wrap

closedTrails :: Ord a => Int -> a -> Graph a -> [[a]]
closedTrails n start graph = trails n graph start start

-- don't backtrack
trails :: Ord a => Int -> Graph a -> a -> a -> [[a]]
trails 0 _ target start = if start == target then [[]] else []
trails n graph target start = concat $ map (map (start:) . trails (n - 1) graph target) (graph start)

solve :: Ord a => Graph a -> a -> Permutation a -> Maybe [a]
solve graph start x = listToMaybe $ do
    n <- [0,2..] -- pull higher
    filter (inverts x . run) $ closedTrails n start graph

tiles :: Tile -> Graph Tile
tiles size = clean size . gen
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

solveTiles :: Ord a => Graph a -> a -> Permutation a -> Maybe [a]
solveTiles graph start x = let xs = alternates x in fmap concat . sequence $ map (solve graph start) xs 

showTile :: Tile -> String
showTile (0,0) = '\x2554' : '\x2550' : [] 
showTile (1,0) = '\x2566' : '\x2566' : [] 
showTile (2,0) = '\x2550' : '\x2557' : [] 
showTile (0,1) = '\x2560' : '\x2550' : [] 
showTile (1,1) = "  "
showTile (2,1) = '\x2550' : '\x2563' : [] 
showTile (0,2) = '\x255a' : '\x2550' : [] 
showTile (1,2) = '\x2569' : '\x2569' : [] 
showTile (2,2) = '\x2550' : '\x255d' : [] 

showTiles :: [Tile] -> String
showTiles = unlines . map concat . chunksOf 3 . map showTile


