
import Data.List

binary :: Int -> [Bool]
binary = unfoldr (\n -> if n==0 then Nothing else Just (odd n, n `div` 2))

toInt :: [Bool] -> Int
toInt = foldr (\b -> (fromEnum b +) . (2*)) 0

prodMod :: Int -> Int -> Int -> Int
prodMod n x = (`mod`n) . (x*)

repeatedSquares :: Int -> Int -> [Int]
repeatedSquares n = iterate ((`mod`n) . (^2))

powerMod' :: Int -> Int -> Int -> Int
powerMod' n base exp = foldr (prodMod n) 1 $ zipFilter (binary exp) (repeatedSquares n base)

powerMod :: Int -> Int -> Int -> Int
powerMod n base exp = powerMod' n (mod base n) (mod exp n)

zipFilter :: [Bool] -> [a] -> [a]
zipFilter bs = map snd . filter fst . zip bs

