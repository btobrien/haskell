

import Data.List
import System.IO
import Utils

import Data.Group.Tile

main = mapM_ putStr . map (showTiles . read) . lines =<< getContents

showTile :: Tile -> String
showTile (0,0) = '\x2554' : '\x2550' : [] 
showTile (1,0) = '\x2566' : '\x2566' : [] 
showTile (2,0) = '\x2550' : '\x2557' : [] 
showTile (0,1) = '\x2560' : '\x2550' : [] 
showTile (1,1) = "  "
showTile (2,1) = '\x2550' : '\x2563' : [] 
showTile (0,2) = '\x255a' : '\x2550' : [] 
showTile (1,2) = '\x2569' : '\x2569' : [] 
showTile (2,2) = '\x2550' : '\x255d' : [] 

showTiles :: [Tile] -> String
showTiles = unlines . map concat . chunksOf 3 . map showTile

