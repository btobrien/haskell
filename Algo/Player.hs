
module Player where

import Data.Either
import Data.List
import Control.Monad
import System.Exit
import Data.Char

granularity = 2
scale :: Double -> Int
scale = floor . (granularity*)
scaleBack :: Int -> Double
scaleBack = (/granularity) . fromIntegral

data Player = Player { result :: Int, name :: String } deriving (Eq, Ord)
instance Read Player where
    readsPrec _ = (\x->[(x,"")]) . (Player <$> scale.read.head.tail <*> head) . words
instance Show Player where show = name <+" "+> show.scaleBack.result

readPlayers :: String -> [Player]
readPlayers = map read . filter (not.all isSpace) . lines

p <++> p' = (++) <$> p <*> p'; infixl 8 <++>
p +> p' = (++) <$> p <*> p'; infixl 8 +>
p <+ p' = (++) <$> p <*> const p'; infixl 8 <+
p ++> p' = (++) <$> const p <*> p'; infixl 8 ++>

printAll :: Show a => [a] -> IO ()
printAll = mapM_ print
printWith shower = mapM_ $ putStrLn . shower

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on process view x y = process (view x) (view y)

-- argmap funky but cool

test = o.o.o $
    (,,,) |----> isLower <---> isSpace <---> isUpper <-> (=='x') 

--binary selection op for subset
o = curry
f <-> v = f <*> v.snd
f <--> v = f <*> v.snd.fst
f |->> v = pure f <*> (uncurry v)
f <|-> v = f <*> v.fst
f |--> v = pure f <*> v.fst
f <---> v = f <*> v.snd.fst.fst
f |---> v = pure f <*> v.fst.fst
f <----> v = f <*> v.snd.fst.fst.fst
f |----> v = pure f <*> v.fst.fst.fst



--pays = o $ Payout |->> amount <|-> name <-> name

