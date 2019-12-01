
module Utils where

import Data.List
import Control.Applicative
import Control.Monad
import qualified Data.Set as S

generate :: Eq a => (a -> a) -> a -> [a]
generate g a = (a:) . takeWhile (/=a) . tail $ iterate g a

powerset = filterM (const [True,False])
pairs xs = (,) <$> xs <*> xs
thruples xs = (,,) <$> xs <*> xs <*> xs
also f x = (x, f x)
ifhead :: ([a] -> b) -> [a] -> Maybe b
ifhead f xs = if null xs then Nothing else Just (f xs)
unfold = unfoldr . ifhead
on process view x y = process (view x) (view y)
over process (viewx,viewy) x y = process (viewx x) (viewy y)
neighbors = zip <$> id <*> tail
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

(=~=) :: Ord a => [a] -> [a] -> Bool
(=~=) = (==) `on` S.fromList

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
chooseFrom = flip choose
    
dump :: Show a => [a] -> IO ()
dump = mapM_ print

dumps :: Show a => [[a]] -> IO ()
dumps xss = putStr . unlines . map (intercalate " ") . (map.map) (pad wid . show)$ xss
    where wid = maximum . map (length.show) . concat $ xss

dumpx :: Show a => [[a]] -> IO ()
dumpx xss = putStr . unlines . addlines . map (intercalate " | ") . (map.map) (pad wid . show)$ xss
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
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . (compare `on`)

-- move Prime into separate module

divisibleBy :: Int -> Int -> Bool
divisibleBy x = (==0) . (`mod`x)

divides :: Int -> Int -> Bool
divides = flip divisibleBy

dividers :: Int -> [Int]
dividers n = filter (divides n) [2..half]
    where half = n `div` 2 

-- sorted
smallestDivider :: Int -> Int
smallestDivider n = head . (++[n]) . dividers $ n

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = (x:) . sieve . filter (not . divisibleBy x) $ xs

primes :: [Int]
primes = sieve [2..]

primesUnder :: Int -> [Int]
primesUnder n = sieve [2..(n-1)]

factor :: Int -> [Int]
factor = unfoldr $ \n -> if n == 1 then Nothing else let d = smallestDivider n in Just (d, n `div` d)

isPrime :: Int -> Bool
isPrime n = factor n == [n]

