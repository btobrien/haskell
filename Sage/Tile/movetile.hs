
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
import Data.Hashable (hash)

data Board = Board { size :: Int, start :: Tile, tiles :: [[Tile]] }

startOf 3 = (1,1)
startOf 4 = (0,0)

withTiles board ts = board { tiles = ts }
modify f board = board { tiles = f (tiles board) }

initial :: Int -> Board
initial n = Board n (startOf n) (chunksOf n . map switch $ pairs [0..(n-1)])

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    n <- read . fromMaybe "3" . listToMaybe <$> getArgs
    dump . map (concat . tiles) . scan move (initial n) =<< getContents

alts 3 = alternating 8
alts 4 = alternating 9

alternate size c = as !! index
    where
    as = alts size
    index = fromIntegral (hash [c]) `mod` length as

move :: Char -> Board -> Board
move 'i' = up
move 'k' = down
move 'l' = right
move 'j' = left
move '0' = \board -> initial (size board)
move c = shuffleBoard c

shuffleBoard :: Char -> Board -> Board
shuffleBoard c board = board `withTiles` shuffledTiles
	where
	n = size board
	shuffledTiles = chunksOf n . shuffleAround (start board) (alternate n c) . concat . tiles $ initial n

up = moveDown
down = modify (rotate 180) . moveDown . modify (rotate 180)
left = modify (rotate 90) . moveDown . modify (rotate 270)
right = modify (rotate 270) . moveDown .  modify (rotate 90)

rotate 90 = reverse . transpose
rotate 180 = map reverse . reverse
rotate 270 = map reverse . transpose

moveDown :: Board -> Board
moveDown board = let ts = tiles board in
	board `withTiles` (take y ts ++ swapTopAt x (drop y ts))
    where
    (x,y) = location (start board) board

location x board = 
    toTile (size board) .
    fromJust .  elemIndex x $ concat (tiles board)

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
