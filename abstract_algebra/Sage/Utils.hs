
module Utils where

import Data.List
import Control.Applicative
import Control.Monad
import qualified Data.Set as S

generate :: Eq a => (a -> a) -> a -> [a]
generate g a = (a:) . takeWhile (/=a) . tail $ iterate g a

powerset = filterM (const [True,False])
pairs xs = (,) <$> xs <*> xs
also f x = (x, f x)
ifhead :: ([a] -> b) -> [a] -> Maybe b
ifhead f xs = if null xs then Nothing else Just (f xs)
unfold = unfoldr . ifhead
on process view x y = process (view x) (view y)
over process (viewx,viewy) x y = process (viewx x) (viewy y)
neighbors = zip <$> id <*> tail
(.:) f g x y = f (g x y) 
p <&&> p' = (&&) <$> p <*> p'; infixl 8 <&&>
p <||> p' = (||) <$> p <*> p'; infixl 8 <||>
p <++> p' = (++) <$> p <*> p'; infixl 8 <++>
p +> p' = (++) <$> p <*> p'; infixl 8 +>
p <+ p' = (++) <$> p <*> const p'; infixl 8 <+

divisibleBy :: Int -> Int -> Bool
divisibleBy x = (==0) . (`mod`x)

divides :: Int -> Int -> Bool
divides = flip divisibleBy

dividers :: Int -> [Int]
dividers n = filter (divides n) [2..half]
    where half = n `div` 2 

(=~=) :: Ord a => [a] -> [a] -> Bool
(=~=) = (==) `on` S.fromList

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
chooseFrom = flip choose
    
dump :: Show a => [a] -> IO ()
dump = mapM_ print


