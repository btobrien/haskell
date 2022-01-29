
-- highlights
-- faster lazy memoization (e.g. tries)
-- efficient construction of one-bite losers
-- recurrence relation computation for 3xN chomp
-- competitive mode: no cheating!

module Chomp where

import Plus 

import Data.Maybe
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

-- generalize for n
barsOf :: Int -> [Bar]
barsOf 3 = 
    [1..] >>= \x ->
    [0..x] >>= \y ->
    [0..y] >>= \z ->
    return $ filter (>0) [x,y,z]

subBars :: Bar -> [Bar]
subBars [] = []
subBars bar = 
    [1..head bar] >>= \x ->
    filter valid . map (x:) $ [] : subBars (tail bar)

winning' :: (Bar -> Bool) -> Bar -> Bool
winning' _ [1] = False
winning' winning bar = any not $ winning . move bar <$> moves bar

winning :: Bar -> Bool
winning = memoize subBars winning'

subLosers :: Bar -> [Bar]
subLosers = Map.keys . Map.filter not . memoized winning' . subBars

losersOf :: Int -> [Bar]
losersOf n =
    filter ((n==).length) . map fst . filter (not.snd) .
    memoized' winning' . barsOf $ n

winningMoves' :: (Bar -> [Move]) -> Bar -> [Move]
winningMoves' _ [1] = []
winningMoves' winningMoves bar = 
    map fst . filter (null.snd) $
    also (winningMoves . move bar) <$> moves bar

winningMoves :: Bar -> [Move]
winningMoves = memoize subBars winningMoves'
                
--

-- finite memo table
memoized :: Ord a => ((a -> b) -> a -> b) -> [a] -> Map a b
memoized fn xs = Map.fromList $ (,)<*> fn (memoized fn xs !) <$> xs

memoize :: Ord a => (a -> [a]) -> ((a -> b) -> a -> b) -> a -> b
memoize subdomain fn = (!) . memoized fn . subdomain <*> id

-- infinite memo table (linear lookup is brutal, switch to inf trie)
memoized' :: Eq a => ((a -> b) -> a -> b) -> [a] -> [(a,b)]
memoized' fn xs = (,)<*> fn (fromJust . flip lookup (memoized' fn xs)) <$> xs

memoize' :: Eq a => [a] -> ((a -> b) -> a -> b) -> a -> b
memoize' xs fn = fromJust . flip lookup (memoized' fn xs)

-- lazily memoized in infinite trie
    -- requires infinite subBars, but...
    -- can't be done because domain is uncountably infinite (!)
    -- but could do it for a fixed width


-- generalize bars
-- infinite trie
-- highlights
-- snap from "outerspace" after computer move
-- understand factor representation
