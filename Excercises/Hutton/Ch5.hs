
grid :: Int -> Int -> [(Int, Int)]
grid a b = [ (x,y) | x <- [0..a], y <-[0..b]]

square :: Int -> [(Int, Int)]
square a = [(x,y) | (x,y) <- grid a a, x /= y]

replicate :: Int -> a -> [a]
replicate n a = [ a | _ <- [1..n] ]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (a, b, c) | a <- [1..n],
						b <- [1..n],
						c <- [1..n],
						a^2 + b^2 == c^2]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (factors x) == 2*x ] 

singen :: a -> [(Int,Int)]
singen _ = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k' == k] 

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

dotprod :: Num a => [a] -> [a] -> a
dotprod xs ys = sum [ fst xys * snd xys | xys <- zip xs ys] 
