

module Data.Group.Units where

import Data.Group
import Data.Group.Modulo
import Data.List
import Control.Applicative
import Data.Group
import Utils
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Maybe
import Data.Prime

isUnit m = fromMaybe True $ do
    b <- baseof m
    let x = fromEnum m
    return $ x /= 0 && gcd x b == 1

newtype Unit = U Modulo; val (U x) = if isUnit x then x else undefined

umodify :: (Modulo -> Modulo) -> Unit -> Unit
umodify f (U x) = U (f x)

ucompose :: (Modulo -> Modulo -> Modulo) -> Unit -> Unit -> Unit
ucompose f (U x) (U y) = U (f x y)

-- should be Enum ???
instance Enum Unit where
    fromEnum = fromEnum . val
    toEnum = undefined

instance Show Unit where
    show = show.val
instance Eq Unit
    where (==) = (==) `on` val
instance Ord Unit
    where compare = compare `on` val
instance Num Unit where
    (+) = ucompose (+)
    (-) = ucompose (-)
    negate = umodify negate
    fromInteger = undefined
    (*) = ucompose (*)
    abs = undefined
    signum = undefined
instance Semigroup Unit  where (<>) = (*)
instance Monoid Unit where
    mempty = U (fromInteger 1)
    mappend = (<>)
instance Group Unit where
    inv x = fromMaybe e $ do
        n <- baseof (val x)
        let x' = fst $ euclidean (fromEnum (val x)) n
        return . U . base n . fromInteger . toInteger $ x'

unit :: Enum a => Int -> a -> Unit
unit n x = let u = base n x in if isUnit u then U u else undefined

units n = [U m | m <- modulo n, isUnit m]

