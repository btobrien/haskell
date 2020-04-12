
import Data.List

(|.) :: Int -> Int -> Bool
(|.) x = (==0) . (`mod`x)

factor :: Int -> [Int]
factor n | n <= 0 = undefined
factor 1 = []
factor n = sort $
    if (2 |. n) then 2 : factor (n `div` 2)
    else factorOdd n

factorOdd :: Int -> [Int]
factorOdd n = case fermat n of
    (1,n) -> [n]
    (a,b) -> factorOdd a ++ factorOdd b

fermat :: Int -> (Int,Int)
fermat n = findDivisor (ceiling . sqrt $ fromIntegral n, 0) n

findDivisor :: (Int,Int) -> Int -> (Int,Int)
findDivisor (x,y) n = let d = n - (x^2 - y^2) in
    if d < 0 then findDivisor (x,y+1) n
    else if d > 0 then findDivisor (x+1,1) n
    else (x-y,x+y)
