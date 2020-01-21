
import System.Environment
import Data.List
import Data.Maybe
import Control.Applicative

main = mapM_ putStrLn . strings . read . fromMaybe "(2,3)" . listToMaybe =<< getArgs

strings :: (Int,Int) -> [String]
strings (b,n) = (map.map) (head.show) . pad . map (expand b) $ [0..((b^n)-1)]

pad :: [[Int]] -> [[Int]]
pad bs = let width = maximum (map length bs) in take width . (++(repeat 0)) <$> bs

expand :: Int -> Int -> [Int]
expand b = unfoldr $ \n ->
    if n==0 then Nothing
    else Just . swap $ quotRem n b

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
