
bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[3,5,2],[5,3,6]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply func = func

second :: [a] -> a
second xs  = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: x -> y -> (x,y)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

