
module Data.Group.Modulo where

import Data.Group
import Data.List
import Control.Applicative
import Data.Group
import Utils

data Modulo = M { baseof :: Int, valueof :: Int }

evaluate :: Modulo -> Int
evaluate x = valueof x `mod` baseof x

base :: Int -> Modulo
base n = M n 0

modify :: (Int -> Int) -> Modulo -> Modulo
modify f x = x { valueof = f.valueof $ x }

compose :: (Int -> Int -> Int) -> Modulo -> Modulo -> Modulo
compose f x y = M (pickbase x y) (valueof x `f` valueof y)
    where pickbase = min `on` baseof

instance Show Modulo where
    show = show.evaluate
instance Eq Modulo
    where (==) = (==) `on` evaluate
instance Ord Modulo
    where compare = compare `on` evaluate
instance Num Modulo where
    (+) = compose (+)
    (-) = compose (-)
    negate = modify negate
    fromInteger = M maxBound . fromIntegral
    (*) = compose (*)
    abs = undefined
    signum = undefined
instance Semigroup Modulo  where (<>) = (+)
instance Monoid Modulo where mempty = fromInteger 0
instance Group Modulo where inv = negate

modulo :: Int -> [Modulo]
modulo n = map (M n) [0..(n-1)]

