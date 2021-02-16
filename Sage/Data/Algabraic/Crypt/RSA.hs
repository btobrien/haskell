
module Data.Crypt.RSA where

import Utils ((<==>), select, dump)
import Data.Prime (prime, primes, isPrime, totient)
import Data.Group (inverse)
import Data.Group.Modulo (Modulo, modulo, baseOf)
import Data.Group.Units (Unit, unit, units)
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as B
import Data.Word

type Seed = Int
type Key = Modulo

generate :: Seed -> (Integer,Integer) -> (Key,Key)
--generate seed (p,q) | prime p && prime q = let
generate seed (p,q) = let
    n = p * q
    m = (p-1) * (q-1)
    e = select seed (units m)
    d = inverse e
    in (modulo n e, modulo n d)

encode :: Key -> Integer -> Integer
encode e x = toInteger $ (modulo (baseOf e) x) ^ e

decode :: Key -> Integer -> Integer
decode = encode

matching :: (Key,Key) -> Bool
matching (e,d) = all (decode d . encode e <==> id) [0..(baseOf e)-1]

matchesEvery :: Integer -> (Key,Key) -> Bool
matchesEvery k (e,d) = all (decode d . encode e <==> id) [0,k..((baseOf e)-1)]

crack :: Key -> Key
crack e = let n = baseOf e in
    modulo n . inverse . unit (totient n) $ e
