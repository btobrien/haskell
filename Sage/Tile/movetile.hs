
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

type Board = ((Int,Tile), [[Tiles]])
size = (fst.fst); center = (fst.snd); tiles = snd 

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    dump . scan (move 3) (initial 3) =<< getContents

alts 3 = alternating 8
alts 4 = alternating 8

alternate size c = as !! index
	where
	as = alts size
	index = fromIntegral (hashString [c]) `mod` length as
	-- note: hashString is deprecated...

center = (1,1)

move :: Char -> Board -> Board
move 'i' = up size
move 'k' = down size
move 'l' = right size
move 'j' = left size
move '0' = const $ initial size
move c = \board -> let n = size board in
	shuffleAround (center board) (alternate n c) (initial n)
	
up size = slide size id id
down size = slide size (rotate 180) (rotate 180)
right size = slide size (rotate 90) (rotate 270)
left size = slide size (rotate 270) (rotate 90)

rotate 90 = reverse . transpose
rotate 180 = map reverse . reverse
rotate 270 = map reverse . transpose

slide size f f' = f' . moveDown size . f .

moveDown :: Int -> Board -> Board
moveDown ts =
	take y ts ++ swapTopAt x (drop y ts)
    where
    (x,y) = location (center board) board

location x board = 
    toTile (size board) .
    fromJust .
    elemIndex x $ concat (tiles board)

toTile :: Int -> Int -> Tile
toTile size x = switch (quotRem x size)

swapTopAt :: Int -> [[a]] -> [[a]] 
swapTopAt _ (xs:[]) = [xs]
swapTopAt n (xs:ys:rest) =
    replace n y xs :
    replace n x ys :
    rest
    where
    x = xs !! n
    y = ys !! n

initial :: Int -> [Tile]
initial n = chunksOf n . map switch $ pairs [0..(n-1)]


