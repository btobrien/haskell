
module Data.Group.Tile where

import Data.List
import Control.Applicative
import Data.Maybe
import Data.Hashable (hash)

import Utils
import Data.Group.Permutation

type Tile = (Int,Int)

type Board = [[Tile]]
width = length . head
height = length
size = (,) <$> width <*> height

cursor :: Tile
cursor = (0,0)

toTile :: (Int,Int) -> Int -> Tile
toTile (w,_) i = swap . quotRem i $ w

correctAt :: Board -> Tile -> Bool
correctAt board x = location x board == x

-- could just compare with solution...
solved :: Board -> Bool
solved board = all (correctAt board) . concat $ board

corner :: Board -> Tile
corner = (\(x,y) -> (x-1,y-1)) . size

location x board = 
    toTile (size board) .
    fromJust . elemIndex x .
    concat $ board

count :: Board -> Int
count = uncurry (*) . size

initial :: (Int,Int) -> Board
initial (w,h) = chunksOf w $
    [toTile (w,h) t | t <- [0..(w*h-1)]]

shuffleBoard :: Char -> Board -> Board
shuffleBoard c board = shuffledTiles
    where
    selectLevel board n = selectEvenPermutation n [1..(count board - 1)]
    shuffledTiles =
        chunksOf (width board) .
        shuffleAround cursor (selectLevel board . hash $ [c]) .
        concat $ board

configure :: Char -> (Int,Int) -> Board
configure c = shuffleBoard c . initial

up = moveDown
down = (rotate' 180) . moveDown . (rotate' 180)
left = (rotate' 90) . moveDown . (rotate' 270)
right = (rotate' 270) . moveDown .  (rotate' 90)

moveDown :: Board -> Board
moveDown ts = take y ts ++ swapTopAt x (drop y ts)
    where
    (x,y) = location cursor ts

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

