
import Data.Maybe
import System.IO
import System.Environment
import Control.Applicative
import System.Exit
import Control.Monad
import Data.List

import Hanoi

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    size <- read . fromMaybe "5" . listToMaybe <$> getArgs
    boards <- takeUntil won . scanMoves (start size) . parsemoves <$> getContents
    mapM_ print boards
    when (not.won.last $ boards) (exitWith.ExitFailure $ 1)

type Move = (Int,Int)

parsemoves :: String -> [Move]
parsemoves =
    map ((,) <$> head <*> head.tail) .  chunksOf 2 .
    map (\c -> fromEnum c - fromEnum 'j') .
    filter (\c -> c == 'j' || c == 'k' || c == 'l')

scanMoves :: Board -> [Move] -> [Board]
scanMoves = scanl (flip move)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = (\(front,back) -> front ++ take 1 back) . break p

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if null xs then Nothing else Just (take n xs, drop n xs))

