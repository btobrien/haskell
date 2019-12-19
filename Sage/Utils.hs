
module Utils where

import Data.List
import Control.Applicative
import Control.Monad
--import Control.Parallel
import qualified Data.Set as Set
import Data.Set (Set)

generate :: Eq a => (a -> a) -> a -> [a]
generate g a = (a:) . takeWhile (/=a) . tail $ iterate g a

mapp = map . uncurry
powerset = filterM (const [True,False])
powerset' = filterM (const [False,True])
pairs xs = (,) <$> xs <*> xs
thruples xs = (,,) <$> xs <*> xs <*> xs
also f x = (x, f x)
ifhead :: ([a] -> b) -> [a] -> Maybe b
ifhead f xs = if null xs then Nothing else Just (f xs)
unfold = unfoldr . ifhead
on process view x y = process (view x) (view y); infixr 8 `on`
over process (viewx,viewy) x y = process (viewx x) (viewy y)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if null xs then Nothing else Just (take n xs, drop n xs))

neighbors :: [a] -> [(a,a)]
neighbors = zip <$> id <*> tail

wrap :: [a] -> [a]
wrap [] = []
wrap (x:xs) = (x:xs) ++ [x]

switch (x,y) = (y,x)

replace :: Int -> a -> [a] -> [a]
replace n x xs = take n xs ++ [x] ++ drop (n+1) xs

insertAt n x xs = take n xs ++ [x] ++ drop n xs

squash :: (a -> Bool) -> a -> Maybe a
squash isNothing x = if isNothing x then Nothing else Just x

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = (\(front,back) -> front ++ take 1 back) . break p

equal :: Eq a => (a,a) -> Bool
equal = uncurry (==)

scan f = scanl (flip f)

convergence :: Eq a => [a] -> a
convergence = fst . head . dropWhile (not.equal) . neighbors

(.:) f g x y = f (g x y) 
p <&&> p' = (&&) <$> p <*> p'; infixl 1 <&&>
p <||> p' = (||) <$> p <*> p'; infixl 1 <||>
p <++> p' = (++) <$> p <*> p'; infixl 1 <++>
p <==> p' = (==) <$> p <*> p'
p +> p' = (++) <$> p <*> p'; infixl 1 +>
p <+ p' = (++) <$> p <*> const p'; infixl 1 <+
p ++> p' = (++) <$> const p <*> p'; infixl 1 ++>
--p |> p' = (++) <$> p <*> show.p'; infixl 1 |>
--p <| p' = (++) <$> p <*> const p'; infixl 1 <|
--p ||> p' = (++) <$> const p <*> show.p'; infixl 1 ||>

infixr 5 +|+
(+|+) :: [a] -> [a] -> [a]
--xs +|+ ys = length ys `par` (xs ++ ys)
(+|+) = (++)

(=~=) :: Ord a => [a] -> [a] -> Bool
(=~=) = (==) `on` Set.fromList

(<~) :: Ord a => [a] -> [a] -> Bool
(<~) = Set.isSubsetOf `on` Set.fromAscList
--(<~) = isSubsequenceOf

normalize :: Ord a => [a] -> [a] 
normalize = Set.toList . Set.fromList

choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose _ []     = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
    
pack :: Eq a => Int -> [[a]] -> [[a]]
pack = map concat .: pack'
    where
    pack' n _ | n < 0 = []
    pack' 0 _         = [[]]
    pack' _ []        = []
    pack' n (xs:xss)  = map (xs:) chosen ++ notChosen
        where
        chosen = pack' (n - length xs) (filter (null . intersect xs) xss)
        notChosen = pack' n xss

dump :: Show a => [a] -> IO ()
dump = mapM_ print

dumps :: Show a => [[a]] -> IO ()
dumps xss = putStr . unlines . map (intercalate " ") . (map.map) show $ xss

dumpss :: Show a => [[[a]]] -> IO ()
dumpss = mapM_ dumps

dumpx :: Show a => [[a]] -> IO ()
dumpx xss = putStr . unlines . map (intercalate " ") . (map.map) (pad wid . show)$ xss
    where wid = maximum . map (length.show) . concat $ xss

dumpt :: Show a => [[a]] -> IO ()
dumpt xss = putStr . unlines . addlines . map (intercalate " | ") . (map.map) (pad wid . show)$ xss
    where
    wid = maximum . map (length.show) . concat $ xss
    addlines ss = let n = length (head ss) in intersperse (replicate n '-') ss

___ = putStrLn 
__ = putStrLn ""
____ = putStr

pad n xs = take n $ xs ++ repeat ' ' 

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy.((==)`on`)

-- we have more efficient impls of these
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' = sortBy . (compare `on`)

-- create separate prime module?

divisibleBy :: Int -> Int -> Bool
divisibleBy x = (==0) . (`mod`x)

divides :: Int -> Int -> Bool
divides = flip divisibleBy

divisors :: Int -> [Int]
divisors n = filter (divides n) [2..(n `div` 2)]

-- assumes divisors is sorted
smallestDivisor :: Int -> Int
smallestDivisor n = head . (++[n]) . divisors $ n

largestDivisor :: Int -> Int
largestDivisor n = last . divisors $ n

primes :: [Int]
primes = sieve [2..]
    where
    sieve [] = []
    sieve (x:xs) = (x:) . sieve . filter (not . divisibleBy x) $ xs

factor :: Int -> [Int]
factor = unfoldr $ \n ->
    if n == 1
    then Nothing
    else let d = smallestDivisor n in Just (d, n `div` d)

isPrime :: Int -> Bool
isPrime n = factor n == [n]

check g assumption f =
    if assumption g
    then Just (f g)
    else Nothing

