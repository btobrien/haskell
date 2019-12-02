

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

isUnit m = fromMaybe True $ do
    b <- baseof m
    let x = evaluate m
    return $ x /= 0 && gcd x b == 1

euclidean :: Int -> Int -> (Int,Int)
euclidean a b = euclidean' (a,b) (1,0) (0,1)
    where
    euclidean' base a b =
        if dot base b == 0 then a
        else euclidean' base b r
        where
        r = a `plus` scale (-q) b
        q = div (dot base a) (dot base b)
        dot (a,b) (r,s) = (a*r) + (b*s)
        plus (a,b) (r,s) = (a+r,b+s)
        scale k (a,b) = (k*a,k*b)

newtype Unit = U Modulo; val (U x) = if isUnit x then x else undefined

umodify :: (Modulo -> Modulo) -> Unit -> Unit
umodify f (U x) = U (f x)

ucompose :: (Modulo -> Modulo -> Modulo) -> Unit -> Unit -> Unit
ucompose f (U x) (U y) = U (f x y)

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
    fromInteger = U . fromInteger
    (*) = ucompose (*)
    abs = undefined
    signum = undefined
instance Semigroup Unit  where (<>) = (*)
instance Monoid Unit where
    mempty = fromInteger 1
    mappend = (<>)
instance Group Unit where
    inv x = fromMaybe e $ do
        n <- baseof (val x)
        let x' = fst $ euclidean (evaluate (val x)) n
        return . U . base n . fromInteger . toInteger $ x'

unit n x = if isUnit (val u) then u else undefined
    where u = U (base n 1) <> x

units n = [U m | m <- modulo n, isUnit m]
