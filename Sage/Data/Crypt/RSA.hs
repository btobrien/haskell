
module RSA where

import Data.Group
import Data.Group.Modulo
import Data.Group.Units
import Data.Prime

type Seed = Int
type Key = Modulo

-- p & q assumed to be prime
generate :: Seed -> (Int,Int) -> (Key,Key)
generate s (p,q) = let
	n = p * q
	m = (p-1) * (q-1)
	e = units m !! (s `mod` m)
	d = inv e
	in (base n e, base n d)

encode :: Key -> Int -> Int
encode e x = fromEnum $ base (baseOf e) x ^ e

decode :: Key -> Int -> Int
decode = encode

crack :: Key -> Key
crack e = let
	n = baseOf e
	[p,q] = factor n
	m = (p-1) * (q-1)
	d = inv (unit m e)
	in base n d
