
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
    dump . scan (move 3) (initial 3) =<< getContents

alts 3 = alternating 8
alts 4 = alternating 8

alternate size c = as !! index
	where
	as = alts size
	index = fromIntegral (hashString [c]) `mod` length as
	-- note: hashString is deprecated...

center = (1,1)

move :: Int -> Char -> [Tile] -> [Tile]
move size 'i' = up size
move size 'k' = down size
move size 'l' = right size
move size 'j' = left size
move size '0' = const $ initial size
move size c = const $ shuffleAround center (alternate size c) (initial size)
	
up size = slide size id id
down size = slide size (rotate 180) (rotate 180)
right size = slide size (rotate 90) (rotate 270)
left size = slide size (rotate 270) (rotate 90)

rotate 90 = reverse . transpose
rotate 180 = map reverse . reverse
rotate 270 = map reverse . transpose

slide size f f' = concat .
    f' . moveDown size . f .
    chunksOf size

moveDown :: Int -> [[Tile]] -> [[Tile]]
moveDown size ts =
	take y ts ++ swapTopAt x (drop y ts)
    where
    (x,y) = location size center ts

location size x xs = 
    toTile size .
    fromJust .
    elemIndex x $ concat xs

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
initial n = map switch $ pairs [0..(n-1)]


