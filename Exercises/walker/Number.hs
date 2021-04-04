import Data.List

funnum = 5

f :: Int -> Int
f x = 2 * x

add :: Int -> Int -> Int
add x y = x + y

addbad :: (Int,Int) -> Int
addbad (x,y) = x + y

ourlist :: [Int]
ourlist = [1,2,3,4]

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 xs = head xs + sum2 (tail xs)

--foldr2 :: [Int] -> Int
foldr2 _ base [] = base
foldr2 comp base xs = comp (head xs) (foldr2 comp base (tail xs))

prod2 :: [Int] -> Int
prod2 [] = 1
prod2 xs = head xs * sum2 (tail xs)

max2 :: [Int] -> Int
max2 [x] = x
max2 xs =let
    a = head xs
    b = max2 (tail xs)
    in
    if a > b then a else b





