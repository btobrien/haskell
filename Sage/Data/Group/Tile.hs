
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

tileGraph :: Tile -> Graph Tile
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

--solveTiles :: Ord a => Graph a -> a -> Permutation a -> Maybe [a]
--solveTiles graph start x = let xs = alternates x in fmap concat . sequence $ map (solve graph start) xs 

showTiles :: [Tile] -> String
showTiles ts = let n =  floor . sqrt . fromIntegral . length $ ts in
	unlines . map concat . chunksOf n . map (showTile n) $ ts


showTile :: Int -> Tile -> String
showTile 3 (0,0) = '\x2554' : '\x2550' : [] 
showTile 3 (1,0) = '\x2566' : '\x2566' : [] 
showTile 3 (2,0) = '\x2550' : '\x2557' : [] 
showTile 3 (0,1) = '\x2560' : '\x2550' : [] 
showTile 3 (1,1) = "  "
showTile 3 (2,1) = '\x2550' : '\x2563' : [] 
showTile 3 (0,2) = '\x255a' : '\x2550' : [] 
showTile 3 (1,2) = '\x2569' : '\x2569' : [] 
showTile 3 (2,2) = '\x2550' : '\x255d' : [] 

--showTile 4 (0,0) = '\x2554' : '\x2550' : [] 
showTile 4 (0,0) = "  "
showTile 4 (1,0) = '\x2566' : '\x2550'  : [] 
showTile 4 (2,0) = '\x2566' : '\x2566' : [] 
showTile 4 (3,0) = '\x2550' : '\x2557' : [] 
showTile 4 (0,1) = '\x255f' : '\x2501' : [] 
showTile 4 (1,1) = '\x256b' : '\x2501' : [] 
showTile 4 (2,1) = '\x256b' : '\x256b' : [] 
showTile 4 (3,1) = '\x2501' : '\x2562' : [] 
showTile 4 (0,2) = '\x2560' : '\x2550' : [] 
showTile 4 (1,2) = '\x256c' : '\x2550' : [] 
showTile 4 (2,2) = '\x256c' : '\x256c' : [] 
showTile 4 (3,2) = '\x2550' : '\x2563' : [] 
showTile 4 (0,3) = '\x255a' : '\x2550' : [] 
showTile 4 (1,3) = '\x2569' : '\x2550' : [] 
showTile 4 (2,3) = '\x2569' : '\x2569' : [] 
showTile 4 (3,3) = '\x2550' : '\x255d' : [] 

