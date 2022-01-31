
import Plus

fibs :: [Int]
fibs = fib (fibs !!) <$> [0..]
    where
    fib _ 0 = 0
    fib _ 1 = 1
    fib fib n = fib (n-1) + fib (n-2)

pascals :: [[Int]]
pascals = flip iterate [1] $ (1:).(++[1]).(zipWith (+) <*> tail)
