
module Plus where

import Data.List
import Control.Applicative

compose :: [(a -> a)] -> (a -> a)
compose = foldr (.) id

also f x = (x, f x)

ifhead :: ([a] -> b) -> [a] -> Maybe b
ifhead f xs = if null xs then Nothing else Just (f xs)

unfold :: ([a] -> (b,[a])) -> [a] -> [b]
unfold = unfoldr . ifhead

chunksOf :: Int -> [a] -> [[a]]
chunksOf = unfold . splitAt

scan :: (a -> b -> b) -> b -> [a] -> [b]
scan = scanl . flip

fold :: (a -> b -> b) -> b -> [a] -> b
fold = foldl . flip

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = (\(front,back) -> front ++ take 1 back) . break p

on process view x y = process (view x) (view y); infixr 8 `on`
over process (viewx,viewy) x y = process (viewx x) (viewy y)

neighbors :: [a] -> [(a,a)]
neighbors = zip <$> id <*> tail

equal :: Eq a => (a,a) -> Bool
equal = uncurry (==)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = take n xs ++ [x] ++ drop (n+1) xs

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt n f xs = let (left,(x:right)) = splitAt n xs
    in
    left ++ [f x] ++ right

dump :: Show a => [a] -> IO ()
dump = mapM_ print

dumps :: Show a => [[a]] -> IO ()
dumps xss = putStr . unlines . map (intercalate " ") . (map.map) show $ xss

(.:) f g x y = f (g x y) 

p <&&> p' = (&&) <$> p <*> p'; infixl 1 <&&>
p <||> p' = (||) <$> p <*> p'; infixl 1 <||>
p <++> p' = (++) <$> p <*> p'; infixl 2 <++>
p <:> p' = (:) <$> p <*> p'; infixl 2 <:>
p <==> p' = (==) <$> p <*> p'; infixl 2 <==>
p </=> p' = (/=) <$> p <*> p'; infixl 2 </=>
p +> p' = (++) <$> p <*> p'; infixl 1 +>
p <+ p' = (++) <$> p <*> const p'; infixl 1 <+
p ++> p' = (++) <$> const p <*> p'; infixl 1 ++>

