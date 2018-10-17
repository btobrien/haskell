
module Ch7 where
import Ch4 hiding ((&&),(||))

-- [ f x | x <- xs, p s]
malter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
malter f p xs = map f (filter p xs)

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs
-- and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs
-- or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if (p x) then (x:takeWhile p xs) else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) = if (p x) then (dropWhile p xs) else (x:xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x -> ((f x):)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x -> if (p x) then (x:) else id) [] 

dec2int :: [Int] -> Int
dec2int = foldl (\x -> (x*10+)) 0

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b] 
unfold p h t x | p x = []
			   | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (==0) (`mod`2) (`div`2)

chop :: Int -> [Int] -> [[Int]]
chop n = unfold null (take n) (drop n)

iterate' :: (a -> a) -> a -> [a] 
iterate' f = unfold (\_ -> False) id f

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

altmap :: (a -> a) -> [a] -> [a]
altmap _ [] = []
altmap _ [x] = [x]
altmap f (x:x1:xs) = x : f x1 : altmap f xs

luhnCheck :: [Int] -> Bool
luhnCheck xs = luhnSum xs `mod` 10 == 0
	where luhnSum = sum . altmap luhnDouble . reverse 
