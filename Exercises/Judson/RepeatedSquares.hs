
import Data.List

binary :: Int -> [Bool]
binary = unfoldr (\n -> if n==0 then Nothing else Just (odd n, n `div` 2))

fromBinary :: [Bool] -> Int
fromBinary = foldr (\b -> (fromEnum b +) . (2*)) 0

prodMod :: Int -> Int -> Int -> Int
prodMod n x = (`mod`n) . (x*)

repeatedSquares :: Int -> Int -> [Int]
repeatedSquares n = iterate ((`mod`n) . (^2))

powerMod :: Int -> Int -> Int -> Int
powerMod n base exp = foldr (prodMod n) 1 $
    zipFilter (binary exp) (repeatedSquares n base)

zipFilter :: [Bool] -> [a] -> [a]
zipFilter bs = map snd . filter fst . zip bs

