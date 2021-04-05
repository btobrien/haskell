
module Data.Algabraic.Crypt.RSA where

import Utils ((<==>))
import Data.Prime (factor, prime, primes, isPrime, totient)
import Data.Group (inverse)
import Data.Group.Modulo (Modulo, modulo, baseOf)
import Data.Group.Units (Unit, unit)
import Data.Char (ord)

import Data.Digest.Pure.SHA as SHA
import Data.ByteString.Lazy (ByteString, pack)

import Numeric (showHex, showIntAtBase)

type Prime = Integer
type Key = Modulo
type PublicKey = Key
type PrivateKey = Key

-- a commonly used encoder because of short bit length and small hamming-weight
defaultEncoder :: Prime
defaultEncoder = 2^16 + 1

smallEncoder :: Prime
smallEncoder = 3

bitLength :: Integer -> Int
bitLength = (+1) . floor . logBase 2 . fromIntegral

generate :: Integer -> [Prime] -> (PublicKey,PrivateKey)
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

keycheck :: Integer -> (Key,Key) -> Bool
keycheck k (e,d) = all (decode d . encode e <==> id) [0,(baseOf e `div` k)..((baseOf e)-1)]

crack :: Key -> Key
crack e = let n = baseOf e in
    modulo n . inverse . unit (totient n) $ e

--
-- Digital Signatures
--

type ShaId = Int
hash :: Show a => ShaId -> a -> Integer
hash n = sha n . pack . map (fromIntegral . ord) . show
    where
    sha 1   = integerDigest . sha1
    sha 224 = integerDigest . sha224
    sha 256 = integerDigest . sha256
    sha 512 = integerDigest . sha512
    sha _ = error "sha: length not supported"

type Signed a = (a,(Integer,PublicKey,ShaId)); valueOf = fst; signatureOf = snd

signer :: Signed a -> Integer
signer = baseOf . (\(_,x,_) -> x) . signatureOf

sign :: Show a => ShaId -> (PublicKey,PrivateKey) -> a -> Signed a
sign sha (pk,sk) message = let
    signature = encode sk . hash sha $ message
    in
    (message,(signature,pk,sha))

verify :: Show a => Signed a -> Bool
verify (message,(signature,pk,sha)) = hash sha message == decode pk signature

example :: String -> Bool
example = verify . sign 256 (generate defaultEncoder (rsa 100))

rsa 100 = [37975227936943673922808872755445627854565536638199,40094690950920881030683735292761468389214899724061]
