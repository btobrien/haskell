
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
instance Group Bool where inverse = id 

instance Num Bool where
    (+) = (/=)
    (-) = (+)
    negate = id
    fromInteger = toEnum . fromIntegral
    -- important to force evaluation
    (*) = (&&)
    abs = id
    signum = undefined

data Modulo = M { baseof :: Maybe Integer, valueof :: Integer }

evaluate :: Modulo -> Integer
evaluate x = fromMaybe (valueof x) $ mod (valueof x) <$> baseof x

forceModulo :: Modulo -> Modulo
forceModulo m = M (baseof m) (fromMaybe (valueof m) $ mod (valueof m) <$> baseof m)

modify :: (Integer -> Integer) -> Modulo -> Modulo
modify f x = x { valueof = f.valueof $ x }

compose :: (Integer -> Integer -> Integer) -> Modulo -> Modulo -> Modulo
compose f x y = M (checkbase x y) (valueof x `f` valueof y)
    where
    checkbase a b = if (baseof a <==> baseof b) == (Just False)
        then undefined
        else baseof a <|> baseof b

instance Enum Modulo where
    fromEnum x = fromInteger . fromMaybe (valueof x) $ mod (valueof x) <$> baseof x
    toEnum = M Nothing . toInteger
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
    fromInteger = M Nothing
    -- important to force evaluation -- why??..
    (*) = compose (*) `on` forceModulo
    abs = id
    signum = undefined

instance Real Modulo where
    toRational = toRational . evaluate

instance Integral Modulo where
    quotRem x y = (quot x y, rem x y)
    quot = compose quot `on` forceModulo
    rem = compose rem `on` forceModulo
    toInteger = evaluate

instance Semigroup Modulo  where (<>) = (+)
instance Monoid Modulo where
    mempty = fromInteger 0
    mappend = (<>)
instance Group Modulo where inverse = negate

modulos :: Integer -> [Modulo]
modulos n = map (M (Just n)) [0..n-1]

modulo n = M (Just n) . toInteger

baseOf = fromJust . baseof

