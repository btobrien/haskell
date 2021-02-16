

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

isUnit :: Modulo -> Bool
isUnit m = fromMaybe True $ do
    b <- baseof m
    let x = toInteger m
    return $ x /= 0 && gcd x b == 1

newtype Unit = U Modulo; val (U x) = x

instance Enum Unit where
    fromEnum = fromEnum . val
    toEnum = undefined

instance Show Unit where
    show = show.val
instance Eq Unit
    where (==) = (==) `on` val
instance Ord Unit
    where compare = compare `on` val
instance Semigroup Unit  where (<>) = U .: (*) `on` val
instance Monoid Unit where
    mempty = U (fromInteger 1)
    mappend = (<>)

instance Group Unit where
    inverse x = fromMaybe identity $ do
        n <- baseof . val $ x
        let x' = fst $ euclidean (toInteger (val x)) n
        return . U . modulo n $ x'

instance Num Unit where
    (+) = U .: (+) `on` val
    (-) = U .: (-) `on` val
    negate = U . negate . val
    fromInteger = U . fromInteger
    (*) = U .: (*) `on` val
    abs = U . abs . val
    signum = U . signum . val

instance Real Unit where
    toRational = toRational . val

instance Integral Unit where
    quotRem x y = (quot x y, rem x y)
    quot = U .: quot `on` val
    rem = U .: rem `on` val
    toInteger = toInteger . val

--unit :: Enum a => Integer -> a -> Unit
unit n x = let u = modulo n x in if isUnit u then U u else undefined

units n = [U m | m <- modulos n, isUnit m]

psuedoprime :: Integer -> Bool
psuedoprime n = if even n then False else
    identity == (unit n 2) .^ (n-1)

psuedoprimeBase :: Unit -> Bool
psuedoprimeBase b = let n = baseOf (val b) in identity == b .^ (n-1)

carmichael :: Integer -> Bool
carmichael = all psuedoprimeBase . units
