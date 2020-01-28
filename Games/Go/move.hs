
import System.IO
import System.Environment
import Data.List
import Data.Maybe
import Control.Applicative

import Go
import Plus (fold)

main = do
    hSetBuffering stdout NoBuffering
    z <- read . fromMaybe "9" . listToMaybe <$> getArgs
    print . fold (uncurry move) (start z) . parsemoves =<< getContents

parsemoves :: String -> [(String, Int)]
parsemoves = map (parsemove . words) . lines

parsemove :: [String] -> (String, Int)
parsemove (cmd:index:_) = (cmd, read index)
parsemove _ = ("", -1) --error (unwords xs)

move :: String -> Int -> Board -> Board
move "n" = place Black
move "m" = place White
move "N" = \n -> (annotate "N" n) . (place Black n)
move "M" = \n -> (annotate "M" n) . (place White n)
move "u" = place Empty
move "i" = annotate "x"
move "I" = annotate "*"
move "o" = annotate ""
move "O" = const clearnotes
move x = annotate x

--write condensed moves to file afterwards
