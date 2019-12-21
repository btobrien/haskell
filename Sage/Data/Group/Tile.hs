
module Data.Group.Tile where

import Data.List
import Control.Applicative
import Data.Maybe
import Data.Hashable (hash)

import Utils
import Data.Group.Permutation

--post-process solution reduction

type Tile = (Int,Int)

data Board = Board { size :: Int, tiles :: [[Tile]] }

withTiles board ts = board { tiles = ts }
modify f board = board { tiles = f (tiles board) }

start :: Tile
start = (0,0)

toTile :: Int -> Int -> Tile
toTile size x = swap (quotRem x size)

location x board = 
    toTile (size board) .
    fromJust .  elemIndex x $ concat (tiles board)

solution :: Int -> Board
solution n = Board n (chunksOf n . map swap $ pairs [0..(n-1)])

shuffleBoard :: Char -> Board -> Board
shuffleBoard c board = board `withTiles` shuffledTiles
    where
    n = size board
    shuffledTiles =
        chunksOf n .
        shuffleAround start (selectEvenPermutation (hash [c]) [1..(n*n - 1)]) .
        concat . tiles $ solution n

up = moveDown
down = modify (rotate' 180) . moveDown . modify (rotate' 180)
left = modify (rotate' 90) . moveDown . modify (rotate' 270)
right = modify (rotate' 270) . moveDown .  modify (rotate' 90)

moveDown :: Board -> Board
moveDown board = let ts = tiles board in
    board `withTiles` (take y ts ++ swapTopAt x (drop y ts))
    where
    (x,y) = location start board

swapTopAt :: Int -> [[a]] -> [[a]] 
swapTopAt _ (xs:[]) = [xs]
swapTopAt n (xs:ys:rest) =
    replace n (ys !! n) xs :
    replace n (xs !! n) ys :
    rest

showTiles :: [Tile] -> String
showTiles ts = let n =  floor . sqrt . fromIntegral . length $ ts in
    unlines . map concat . chunksOf n . map (showTile n) $ ts

showTile :: Int -> Tile -> String

--showTile 3 (0,0) = '\x2554' : '\x2550' : [] 
showTile 3 (0,0) = "  "
showTile 3 (1,0) = '\x2566' : '\x2566' : [] 
showTile 3 (2,0) = '\x2550' : '\x2557' : [] 
showTile 3 (0,1) = '\x2560' : '\x2550' : [] 
showTile 3 (1,1) = '\x256c' : '\x256c' : [] 
showTile 3 (2,1) = '\x2550' : '\x2563' : [] 
showTile 3 (0,2) = '\x255a' : '\x2550' : [] 
showTile 3 (1,2) = '\x2569' : '\x2569' : [] 
showTile 3 (2,2) = '\x2550' : '\x255d' : [] 

showTile 4 (0,0) = "  "
showTile 4 (1,0) = '\x2566' : '\x2550' : [] 
showTile 4 (2,0) = '\x2566' : '\x2566' : [] 
showTile 4 (3,0) = '\x2550' : '\x2557' : [] 
showTile 4 (0,1) = '\x255f' : '\x2500' : [] 
showTile 4 (1,1) = '\x256b' : '\x2500' : [] 
showTile 4 (2,1) = '\x256b' : '\x256b' : [] 
showTile 4 (3,1) = '\x2500' : '\x2562' : [] 
showTile 4 (0,2) = '\x2560' : '\x2550' : [] 
showTile 4 (1,2) = '\x256c' : '\x2550' : [] 
showTile 4 (2,2) = '\x256c' : '\x256c' : [] 
showTile 4 (3,2) = '\x2550' : '\x2563' : [] 
showTile 4 (0,3) = '\x255a' : '\x2550' : [] 
showTile 4 (1,3) = '\x2569' : '\x2550' : [] 
showTile 4 (2,3) = '\x2569' : '\x2569' : [] 
showTile 4 (3,3) = '\x2550' : '\x255d' : [] 

