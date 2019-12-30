
import Data.Maybe
import System.Environment

main = putStrLn . solve . read . fromMaybe "3" . listToMaybe =<< getArgs

solve :: Int -> String
solve 1 = "jl"
solve n = from (1,2) (solve (n-1)) ++ solve 1 ++ from (2,3) (solve (n-1))

from :: (Int,Int) -> String -> String
from (1,2) = map (swap 'l' 'k')
from (2,3) = map (swap 'j' 'k')

swap :: Char -> Char -> Char -> Char
swap a b x =
    if x == a then b
    else if x == b then a
    else x

