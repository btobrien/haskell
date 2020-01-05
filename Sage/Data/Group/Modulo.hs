
module Data.Group.Modulo where

import Data.List
import Control.Applicative
import Data.Group hiding ((^))
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

forceModulo :: Modulo -> Modulo
forceModulo m = M (baseof m) (fromEnum m)

modify :: (Int -> Int) -> Modulo -> Modulo
modify f x = x { valueof = f.valueof $ x }

compose :: (Int -> Int -> Int) -> Modulo -> Modulo -> Modulo
compose f x y = M (checkbase x y) (valueof x `f` valueof y)
    where
    checkbase a b = if (baseof a <==> baseof b) == (Just False)
        then undefined
        else baseof a <|> baseof b

instance Show Modulo where
    show = show.fromEnum
instance Enum Modulo where
    fromEnum x = fromMaybe (valueof x) $ mod (valueof x) <$> baseof x
    toEnum = M Nothing
instance Eq Modulo
    where (==) = (==) `on` fromEnum
instance Ord Modulo
    where compare = compare `on` fromEnum

instance Num Modulo where
    (+) = compose (+)
    (-) = compose (-)
    negate = modify negate
    fromInteger = toEnum . fromIntegral
    -- important to force evaluation
    (*) = compose (*) `on` forceModulo
    abs = undefined
    signum = undefined

instance Real Modulo where
    toRational = toRational . fromEnum

instance Integral Modulo where
    quotRem x y = (quot x y, rem x y)
    quot = compose quot `on` forceModulo
    rem = compose rem `on` forceModulo
    toInteger = toInteger . fromEnum

instance Semigroup Modulo  where (<>) = (+)
instance Monoid Modulo where
    mempty = fromInteger 0
    mappend = (<>)
instance Group Modulo where inv = negate

modulo :: Int -> [Modulo]
modulo n = map (M (Just n)) [0..(n-1)]

base :: Int -> Modulo -> Modulo
base n = (M (Just n) 0 <>)  

-- implements repeated squares
power :: Integral a => a -> Modulo -> Modulo
power exponent = product . zipFilter (binary exponent) . iterate (^2)

