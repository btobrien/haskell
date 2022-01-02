
import System.IO
import Data.List
import Data.Char

import Plus (swap, on)
import Chomp

main = do
    hSetBuffering stdout NoBuffering
    getContents >>= putStr . unlines . map (showBar . read) . lines

coords :: Bar -> [[Move]]
coords = zipWith (map . flip (,)) [0..] . (replicate <$> head <*> flip take [0..] . length)

showBar :: Bar -> String
showBar [] = " "
showBar bar = unlines
    . map (intersperse connect)
    . addEdge
    . map (map (gridAt bar) . filter (not.isOutside bar)) . coords $ bar

addEdge :: [String] -> [String]
addEdge xs = 
     (topLeft : replicate (length (head xs) - 1) top ++ [topRight])
     : map (left:) (init xs)
     ++ [bottomLeft:last xs]

gridAt ::  Bar -> Move -> Char
gridAt bar x | isBottom bar x && isRight bar x = bottomRight
gridAt bar x | isBottom bar x = bottom
gridAt bar x | isRight bar x = right
gridAt bar x | otherwise = intersection

isOutside :: Bar -> Move -> Bool
isOutside bar (x,y) = bar!!x < y+1

isBottom :: Bar -> Move -> Bool
isBottom bar (x,y) = bar!!x == y+1

isRight :: Bar -> Move -> Bool
isRight bar (x,y) = (x+1) == length bar || y+1 > (bar !! (x+1))

top = '\x252c'
right ='\x2524'
bottom = '\x2534' 
left = '\x251c'

topLeft = '\x250c'
topRight = '\x2510'
bottomRight = '\x2518'
bottomLeft = '\x2514'

intersection = '\x253c'
connect = '\x2500'
blank = ' '

