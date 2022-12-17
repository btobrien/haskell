module Plus where

import Data.List
import Control.Applicative

neighbors :: [a] -> [(a,a)]
neighbors = zip <$> id <*> tail

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
takeUntil = (fst <++> take 1 . snd) .: break

also :: (a -> b) -> a -> (a,b)
also = (<*>) (,)

mapAlso = map.also

both :: (a -> b) -> (a,a) -> (b,b)
both fn (x,y) = (fn x, fn y)

swap = \(x,y) -> (y,x)

(.:) f g x y = f (g x y) 
on process view x y = process (view x) (view y); infixr 8 `on`
over process (viewx,viewy) x y = process (viewx x) (viewy y)

groupOn fn = groupBy ((==) `on` fn)

x <<$>> y = (<$>) . x <*> y; infixl 1 <<$>>
x <&&> y = (&&) . x <*> y; infixl 1 <&&>
x <||> y = (||) . x <*> y; infixl 1 <||>
x <++> y = (++) . x <*> y; infixl 2 <++>
x <:> y = (:) . x <*> y; infixl 2 <:>
x <.> y = (.) . x <*> y; infixl 1 <.>
x .> y = (.) . const x <*> y; infixl 1 .>
x <. y = (.) . x <*> const y; infixl 1 <.
x <==> y = (==) . x <*> y; infixl 2 <==>
x <^> y = (,) . x <*> y; infixl 1 <^>
x +> y = (++) . x <*> y; infixl 1 +>
x <+ y = (++) . x <*> const y; infixl 1 <+
x ++> y = (++) . const x <*> y; infixl 1 ++>
x <<> y = (<) . x <*> y; infixl 2 <<>
x <>> y = (>) . x <*> y; infixl 2 <>>
x <+> y = (+) . x <*> y; infixl 2 <+>
x <-> y = (-) . x <*> y; infixl 2 <->
x </> y = (/) . x <*> y; infixl 1 </>

dump :: Show a => [a] -> IO ()
dump = mapM_ print

dumps :: Show a => [[a]] -> IO ()
dumps = putStr . unlines . map unwords . (map.map) show

