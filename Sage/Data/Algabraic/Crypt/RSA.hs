
module Data.Crypt.RSA where

import Data.List (cycle)

import Utils ((<==>))
import Data.Group (inverse)
import Data.Group.Modulo (Modulo, modulo, baseOf)
import Data.Group.Units (unit, units)
import Data.Prime (isPrime, totient)

type Seed = Int
type Key = Modulo

generate :: Seed -> (Int,Int) -> (Key,Key)
generate seed (p,q) | isPrime p && isPrime q = let
    n = p * q
    m = (p-1) * (q-1)
    e = cycle (units m) !! seed
    d = inverse e
    in (modulo n e, modulo n d)

encode :: Key -> Int -> Int
encode e x = fromEnum $ (modulo (baseOf e) x) ^ e

decode :: Key -> Int -> Int
decode = encode

matching :: (Key,Key) -> Bool
matching (e,d) = all (decode d . encode e <==> id) [0..(baseOf e)-1]

crack :: Key -> Key
crack e = let n = baseOf e
    in modulo n . inverse . unit (totient n) $ e
