
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
showdisk width disk = take width $ replicate disk '.' ++ repeat ' '

grid :: Board -> [[Disk]]
grid board = map (topoff size) (disks board)
    where size = sizeOf board

topoff :: Int -> [Disk] -> [Disk]
topoff size ds = replicate (size - length ds) 0 ++ ds

sizeOf :: Board -> Int
sizeOf = maximum . concat . disks

disks :: Board -> [[Disk]]
disks board = left board : center board : right board : []
