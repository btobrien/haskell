
import Data.Maybe
import System.IO
import System.Environment
import Control.Applicative
import System.Exit
import Control.Monad
import Data.List
import Data.Char

import Utils
import Data.Group.Tile
import Data.Group.Permutation

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    n <- read . fromMaybe "3" . listToMaybe <$> getArgs
    dump . map concat . scan move (solution (n,n)) =<< getContents

move :: Char -> Board -> Board
move 'i' = up
move 'k' = down
move 'l' = right
move 'j' = left
move '0' = \board -> solution (size board)
move c | isLower c = id
move c | isUpper c = shuffleBoard c
move c = configure c . size
