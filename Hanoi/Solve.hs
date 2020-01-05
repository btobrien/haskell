
import Data.Maybe
import System.Environment

main = putStrLn . solve . read . fromMaybe "3" . listToMaybe =<< getArgs

solve 1 = "jl"
solve n = swap ('l','k') (solve (n-1)) ++ solve 1 ++ swap ('j','k') (solve (n-1))
    where
    swap (a,b) = map $ \x ->
        if x == a then b
        else if x == b then a
        else x
