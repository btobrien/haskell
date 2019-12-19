
module Idc where

import Data.List
import System.IO
import Utils

import Data.Group.Tile

--main = mapM_ putStr . map (showTiles . read) . lines =<< getContents

showTile :: Int -> Tile -> String

showTile 3 (0,0) = '\x2554' : '\x2550' : [] 
showTile 3 (1,0) = '\x2566' : '\x2566' : [] 
showTile 3 (2,0) = '\x2550' : '\x2557' : [] 
showTile 3 (0,1) = '\x2560' : '\x2550' : [] 
showTile 3 (1,1) = "  "
showTile 3 (2,1) = '\x2550' : '\x2563' : [] 
showTile 3 (0,2) = '\x255a' : '\x2550' : [] 
showTile 3 (1,2) = '\x2569' : '\x2569' : [] 
showTile 3 (2,2) = '\x2550' : '\x255d' : [] 

showTile 4 (0,0) = '\x2554' : '\x2550' : [] 
showTile 4 (1,0) = '\x2564' : '\x2564' : [] 
showTile 4 (2,0) = '\x2566' : '\x2566' : [] 
showTile 4 (3,0) = '\x2550' : '\x2557' : [] 
showTile 4 (0,1) = '\x2560' : '\x2550' : [] 
showTile 4 (1,1) = "  "
showTile 4 (2,1) = '\x2550' : '\x2563' : [] 
showTile 4 (3,1) = '\x2550' : '\x2557' : [] 
showTile 4 (0,2) = '\x255a' : '\x2550' : [] 
showTile 4 (1,2) = '\x2569' : '\x2569' : [] 
showTile 4 (2,2) = '\x2550' : '\x255d' : [] 
showTile 4 (3,2) = '\x2550' : '\x2557' : [] 
showTile 4 (4,0) = '\x2550' : '\x2557' : [] 
showTile 4 (4,1) = '\x2550' : '\x2557' : [] 
showTile 4 (4,2) = '\x2550' : '\x2557' : [] 
showTile 4 (4,3) = '\x2550' : '\x2557' : [] 

showTiles :: [Tile] -> String
showTiles ts = let n =  floor . sqrt . fromIntegral . length $ ts in
	unlines . map concat . chunksOf n . map (showTile n) $ ts


