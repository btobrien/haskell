
import Data.Maybe
import System.IO
import System.Environment
import Control.Applicative
import System.Exit
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe

import Utils
import Data.Group.Tile
import Data.Group.Tile.Solver
import Data.Group.Permutation

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    start <- initial . (\n -> (n,n)) . read . fromMaybe "3" . listToMaybe <$> getArgs
    dump . scan control start =<< getContents

control :: Char -> Board -> Board
control 'i' = up
control 'k' = down
control 'l' = right
control 'j' = left
control 'h' = \board -> fromMaybe id (slide <$> hint board) board
control '0' = initial . size
control c | isLower c = id
control c | isUpper c = shuffleBoard c
control c = initialize c . size
