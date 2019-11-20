
import Data.List
import Hanoi
import System.IO

main = mapM_ putStr . map (showboard . read) . lines =<< getContents

showboard :: Board -> String
showboard board = unlines .
    map (intercalate " ") .
    (map.map) (showdisk (size board)) .
    transpose . grid $ board

showdisk :: Int -> Disk -> String
showdisk width disk = take width $ replicate disk '.' ++ repeat ' '

grid :: Board -> [[Disk]]
grid board = map (topoff . size $ board) (disks board)

topoff :: Int -> [Disk] -> [Disk]
topoff size ds = replicate (size - length ds) 0 ++ ds

size :: Board -> Int
size = maximum . concat . disks

disks :: Board -> [[Disk]]
disks board = left board : center board : right board : []
