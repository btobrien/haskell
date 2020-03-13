
import Data.Maybe
import System.Environment

--performance testing
--main = putStrLn . show . flip div 2 . length . solve' . read . fromMaybe "3" . listToMaybe =<< getArgs
main = putStrLn . solve . read . fromMaybe "3" . listToMaybe =<< getArgs

solve 1 = "jl"
solve n = let sub = solve (n-1) in swap ('l','k') sub ++ solve 1 ++ swap ('j','k') sub
    where
    swap (a,b) = map $ \x ->
        if x == a then b
        else if x == b then a
        else x

solve' = solveWith 'j' 'k' 'l'

solveWith from _ to 1 = [from,to]
solveWith from middle to n = solveWith from to middle (n-1) ++ [from,to] ++ solveWith middle from to (n-1)
