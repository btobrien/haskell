module Pixelate where

import Data.List

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = [take n xs] ++ chunksOf n (drop n xs)

type Grid = [[(Int,Int)]]

grid :: (Int,Int) -> Int -> Grid
grid origin size = reverse . transpose . chunksOf size $ 
    (,) <$>
    [fst origin..fst origin + size - 1]
    <*> [snd origin..snd origin + size - 1]

data Color = Gray | Red | Green | Yellow | Blue | Purple | Cyan | White | Blank
    deriving (Show, Enum, Bounded)

showColors :: [[Color]] -> String
showColors = unlines . map (concatMap pixel)

-- colors string
paint :: Color -> String -> String
paint Blank str = str
paint color str = "\ESC[" ++ show (30 + fromEnum color) ++ "m" ++ str ++ "\ESC[0m"

-- returns a colored "pixel"
pixel :: Color -> String
pixel Blank = "  "
pixel color = "\ESC[" ++ show (40 + fromEnum color) ++ "m  \ESC[0m"
