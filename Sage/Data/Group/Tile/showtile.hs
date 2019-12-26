
import Data.List
import System.IO
import Utils

import Data.Group.Tile

main = mapM_ putStr . map (showBoard . read) . lines =<< getContents

