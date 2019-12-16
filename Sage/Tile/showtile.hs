

import Data.List
import System.IO

import Data.Group.Tile

main = mapM_ putStr . map (showTiles . read) . lines =<< getContents
