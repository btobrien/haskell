
module Data.Crypt.RSA where

import Utils ((<==>))
import Data.Prime (factor, prime, primes, isPrime, totient)
import Data.Group (inverse)
import Data.Group.Modulo (Modulo, modulo, baseOf)
import Data.Group.Units (Unit, unit)

--import Data.Digest.Pure.SHA
--import qualified Data.ByteString.Lazy as B
--import Data.Word

type Prime = Integer
type Key = Modulo

-- a commonly used encoder because of short bit length and small hamming-weight
defaultEncoder :: Prime
defaultEncoder = 2^16 + 1

smallEncoder :: Prime
smallEncoder = 3

bitLength :: Integer -> Int
bitLength = (+1) . floor . logBase 2 . fromIntegral

generate :: Prime -> [Prime] -> (Key,Key)
generate e' pq = let
    n = product pq
    m = product . map (subtract 1) $ pq
    e = unit m e'
    d = inverse e
    in (modulo n e, modulo n d)

-- CRT (ChineseRemainderThm) can be used for more efficent encoding based on factorization...
encode :: Key -> Integer -> Integer
encode e x = toInteger $ (modulo (baseOf e) x) ^ e

decode :: Key -> Integer -> Integer
decode = encode

check :: Integer -> (Key,Key) -> Bool
check k (e,d) = all (decode d . encode e <==> id) [0,(baseOf e `div` k)..((baseOf e)-1)]

crack :: Key -> Key
crack e = let n = baseOf e in
    modulo n . inverse . unit (totient n) $ e
