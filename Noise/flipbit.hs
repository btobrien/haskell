

import System.Environment
import Data.List
import Data.Maybe
import Prelude hiding (flip)

main = do
    errors <- read . fromMaybe "1" . listToMaybe <$> getArgs
    mapM_ putStrLn . concatMap (flip bit errors) . lines =<< getContents

flip :: (a -> a) -> Int -> [a] -> [[a]]
flip f 0 xs      = [xs]
flip f _ []     = []
flip f n (x:xs) =
    map (f x:) (flip f (n-1) xs) ++
    map (x:)   (flip f n xs)
    
bit :: Char -> Char
bit '0' = '1'
bit '1' = '0'
