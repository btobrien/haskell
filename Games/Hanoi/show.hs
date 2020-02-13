
import Data.List
import System.IO

import Hanoi

main = mapM_ putStr . map (showboard . read) . lines =<< getContents

showboard :: Board -> String
showboard board = unlines .
    map (intercalate " ") .
    (map.map) (showdisk (sizeOf board)) .
    transpose . grid $ board

showdisk :: Int -> Disk -> String
showdisk width disk = color disk . pad width disk $ replicate disk '.'

grid :: Board -> [[Disk]]
grid board = map (topoff size) (disks board)
    where size = sizeOf board

topoff :: Int -> [Disk] -> [Disk]
topoff size ds = replicate (size - length ds) 0 ++ ds

sizeOf :: Board -> Int
sizeOf = maximum . concat . disks

disks :: Board -> [[Disk]]
disks board = left board : center board : right board : []

color :: Int -> String -> String
color n str = "\ESC[1;" ++ show (31 + (n `mod` 6)) ++ "m" ++ str ++ "\ESC[0m"

pad :: Int -> Int -> String -> String
pad width 0 str = replicate (2*width) ' '
pad width disk str = let padding = replicate (width - disk) ' ' in padding ++ (' ' : intersperse ' ' str) ++ padding
