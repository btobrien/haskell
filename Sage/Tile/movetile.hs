
import Data.Maybe
import System.IO
import System.Environment
import Control.Applicative
import System.Exit
import Control.Monad
import Data.List

import Utils
import Data.Group.Tile
import Data.Group.Permutation
import Data.HashTable (hashString)

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    str <- getContents
    mapM_ print . scanMoves initial $ str


scanMoves :: [Tile] -> String -> [[Tile]]
scanMoves = scan move

a8 = alternating 8
alternate c = let n = fromIntegral (hashString [c]) `mod` length a8 in a8 !! n

move :: Char -> [Tile] -> [Tile]
move 'i' = up
move 'k' = down
move 'l' = right
move 'j' = left
move '0' = const initial
move c = const $ shuffleAround center (alternate c) initial
	
center = (1,1)

shuffleAround :: Ord a => a -> (Permutation Int) -> [a] -> [a]
shuffleAround x pi xs = fromPermutation px xs
	where
	px = toPermutation . shuffle pi $ delete x xs

up = slide id id
down = slide (rotate 180) (rotate 180)
right = slide (rotate 90) (rotate 270)
left = slide (rotate 270) (rotate 90)

rotate 90 = reverse . transpose
rotate 180 = map reverse . reverse
rotate 270 = map reverse . transpose

slide f f' = concat .
    f' . moveDown . f .
    chunksOf 3

moveDown :: [[Tile]] -> [[Tile]]
moveDown (t:ts) =
    if y == 2 then (t:ts)
    else if y == 1 then t : swapTopAt x ts
    else swapTopAt x (t:ts)
    where
    (x,y) = location center (t:ts)

location x xs = 
    toTile .
    fromJust .
    elemIndex x $ concat xs

toTile :: Int -> Tile
toTile x = switch (quotRem x 3)

swapTopAt :: Int -> [[a]] -> [[a]] 
swapTopAt n (xs:ys:rest) =
    replace n y xs :
    replace n x ys :
    rest
    where
    x = xs !! n
    y = ys !! n


initial :: [Tile]
initial = map switch $ pairs [0..2]


