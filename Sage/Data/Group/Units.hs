

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

newtype Unit = U Modulo; val (U x) = x

umodify :: (Modulo -> Modulo) -> Unit -> Unit
umodify f (U x) = U (f x)

ucompose :: (Modulo -> Modulo -> Modulo) -> Unit -> Unit -> Unit
ucompose f (U x) (U y) = U (f x y)

instance Enum Unit where
    fromEnum = fromEnum . val
    toEnum = undefined

instance Show Unit where
    show = show.val
instance Eq Unit
    where (==) = (==) `on` val
instance Ord Unit
    where compare = compare `on` val
instance Semigroup Unit  where (<>) = ucompose (*)
instance Monoid Unit where
    mempty = U (fromInteger 1)
    mappend = (<>)
instance Group Unit where
    inverse x = fromMaybe identity $ do
        n <- baseof (val x)
        let x' = fst $ euclidean (fromEnum (val x)) n
        return . U . modulo n . fromInteger . toInteger $ x'

unit :: Enum a => Int -> a -> Unit
unit n x = let u = modulo n x in if isUnit u then U u else undefined

units n = [U m | m <- modulos n, isUnit m]

psuedoprime :: Int -> Bool
psuedoprime n = if even n then False else
	identity == (unit n 2) .^ (n-1)

psuedoprimeBase :: Unit -> Bool
psuedoprimeBase b = let n = baseOf (val b) in identity == b .^ (n-1)

carmichael :: Int -> Bool
carmichael = all psuedoprimeBase . units
