module Ch6 where

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)

(^) :: Int -> Int -> Int
(^) m 0 = 1
(^) m n = m * (m Ch6.^ (n-1))

euc :: Int -> Int -> Int
euc m n | m == n = m
		| m >  n = euc (m - n) n 
		| m <  n = euc m (n - m)

and :: [Bool] -> Bool
--and [] = True
--and (b:bs) = b && (Ch6.and bs)
and xs = foldr (&&) True xs

concat :: [[a]] -> [a]
--concat [] = []
--concat (xs:xss) = xs ++ (Ch6.concat xss)
concat xs = foldr (++) [] xs

repl :: Int -> a -> [a]
repl 0 _ = []
repl n a | n > 0 = a:(repl (n-1) a)

(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = xs Ch6.!! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) = a == x || (Ch6.elem a xs) 

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:(merge xs (y:ys))
					| otherwise = y:(merge (x:xs) ys)

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs) 
	where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
	where (left,right) = halve xs

sum' :: Num a => [a] -> a
--sum' [n] = n
--sum' (n:ns) = n + sum' ns
sum' ns = foldr (+) 0 ns

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs) 

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
