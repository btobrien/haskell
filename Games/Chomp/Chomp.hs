
module Chomp where

import Plus 

import Data.List (delete)

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!), Map)

type Bar = [Int]
type Move = (Int,Int)

rectangle :: (Int,Int) -> Bar
rectangle = uncurry replicate

valid :: Bar -> Bool
valid = and . (zipWith (>=) <*> tail)

move :: Bar -> Move -> Bar
move = flip $ \(x,y) -> take x <++> filter (>0) . map (min y) . drop x

moves :: Bar -> [Move]
moves = delete (0,0) . concatMap (map.(,).fst <*> flip take [0..] . snd) . zip [0..]

allBars :: Bar -> [Bar]
allBars [] = []
allBars bar = 
    [1..head bar] >>= \x ->
    filter valid . map (x:) $ [] : allBars (tail bar)

-- unmemoized
winning_ :: Bar -> Bool
winning_ [1] = False
winning_ bar = any not $ winning_ . move bar <$> moves bar

isWinning :: (Bar -> Bool) -> Bar -> Bool
isWinning _ [1] = False
isWinning winning bar = any not $ winning . move bar <$> moves bar

winning :: Bar -> Bool
winning = memoize allBars isWinning

allLosers :: Bar -> [Bar]
allLosers = Map.keys . Map.filter not . memoized isWinning . allBars

winningMoves :: Bar -> [Move]
winningMoves = memoize allBars win
    where
    win _ [1] = []
    win winningMoves bar = map fst . filter (null.snd) $ also (winningMoves . move bar) <$> moves bar
                
--

memoized :: Ord a => ((a -> b) -> a -> b) -> [a] -> Map a b
memoized fn xs = Map.fromList $ (,)<*> fn (memoized fn xs !) <$> xs

memoize :: Ord a => (a -> [a]) -> ((a -> b) -> a -> b) -> a -> b
memoize gen fn = (!) . memoized fn . gen <*> id

-- solutions:
-- unmemoized
-- lazily memoized in finite tree
    -- requires efficient allBars
-- lazily memoized in infinite trie
    -- requires infinite allBars, but...
    -- can't be done because domain is uncountably infinite (!)
-- monadically memoized
-- mutable data-structure / hash table?

