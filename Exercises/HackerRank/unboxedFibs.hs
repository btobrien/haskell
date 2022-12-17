
import Data.Array.Unboxed

main = getContents >>= putStr . unlines .
    map (show . (fibs 10000 !) . read)
    . drop 1 . lines 

fibs :: Int -> Array Int Int
fibs n = listArray (0,n) . reverse . head . drop n . iterate fib $ [1,0]
    where
    fib (a:b:xs) = ((a+b)`mod`modulus):a:b:xs
    modulus = 10^8 + 7

