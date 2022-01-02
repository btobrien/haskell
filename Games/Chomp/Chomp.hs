
module Chomp where

import Plus 

import Data.Maybe (isNothing, listToMaybe)
import Data.List (sortOn, delete, nub)
import Data.Bool (bool)

import Data.Map (Map)
--import qualified Data.Map as Map

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!), Map)

type Bar = [Int]
type Move = (Int,Int)

winning' :: Bar -> Bool
winning' [1] = False
winning' bar = or . map (not . winning' . move bar) . moves $ bar

winners' :: Bar -> [Move]
winners' [1] = []
winners' bar = map fst . filter (null.snd) . (map.fmap) winners' . subars $ bar
    where
    subars = also . move <<$>> moves
                
move :: Bar -> Move -> Bar
move = flip $ \(x,y) -> take x <++> filter (>0) . map (min y) . drop x

moves :: Bar -> [Move]
moves = delete (0,0) . concatMap ((,).fst <<$>> flip take [0..] . snd) . zip [0..]

reflect :: Bar -> Bar
reflect = undefined

-- generalize for n
bars :: Int -> [[Int]]
bars 3 = 
    [1..] >>= \x ->
    [1..x] >>= \y ->
    [1..y] >>= \z ->
    return [x,y,z]

losers :: Int -> [Bar]
losers = filter (not.winning) . bars

rectangle :: (Int,Int) -> Bar
rectangle = uncurry replicate

type Result = Maybe (Move,Int) -- include distance

choices :: Bar -> [Bar]
choices = move <<$>> moves

subbars :: Bar -> [Bar]
subbars [1] = []
subbars bar = choices bar >>= id <:> subbars

singles :: Bar -> [Bar]
singles = undefined

--cliffs :: Bar -> [Int]
--cliffs = \bar -> zip [0..] $ zipWith (==) bar (tail bar ++ [0]) 

winning bar = win bar bar
--winning = winning'

win :: Bar -> Bar -> Bool
win bar = (!) . fmap (win' bar) . Map.fromList . map (id<^>id) . (bar:) . subbars $ bar
    where 
    win' _ [1] = False
    win' size bar = or . map (not . win size . move bar) . moves $ bar




