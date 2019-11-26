
module Data.Group.Modulo where

import Data.Group
import Data.List
import Control.Applicative
import Data.Group
import Utils
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Maybe

instance Semigroup Bool
    where 
    True <> True = False
    False <> False = False
    _ <> _ = True
instance Monoid Bool where
	mempty = False 
	mappend = (<>)
instance Group Bool where inv = id 

data Modulo = M { baseof :: Maybe Int, valueof :: Int }

evaluate :: Modulo -> Int
evaluate x = fromMaybe (valueof x) $ mod (valueof x) <$> baseof x

modify :: (Int -> Int) -> Modulo -> Modulo
modify f x = x { valueof = f.valueof $ x }

compose :: (Int -> Int -> Int) -> Modulo -> Modulo -> Modulo
compose f x y = M (checkbase x y) (valueof x `f` valueof y)
    where
	checkbase a b = if (baseof a <==> baseof b) == (Just False)
		then undefined
		else baseof a <|> baseof b

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
    fromInteger = M Nothing . fromIntegral
    (*) = compose (*)
    abs = undefined
    signum = undefined
instance Semigroup Modulo  where (<>) = (+)
instance Monoid Modulo where
	mempty = fromInteger 0
	mappend = (<>)
instance Group Modulo where inv = negate

modulo :: Int -> [Modulo]
modulo n = map (M (Just n)) [0..(n-1)]

base :: Int -> Modulo -> Modulo
base n = (M (Just n) 0 <>)  
