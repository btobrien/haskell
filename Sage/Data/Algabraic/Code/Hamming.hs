

module Data.Algabraic.Code.Hamming where

import Data.List
import Data.Matrix
import Control.Applicative ((<$>))

import Data.Group.Modulo ()
import Utils (binary, padBinary, sortOn', chunksOf, (.:))
import Data.Algabraic.Code (Code)

readBinary :: String -> [Bool]
readBinary = map (=='1')

showBinary :: [Bool] -> String
showBinary = concatMap (show . fromEnum)

hamming :: Code
hamming = let wrap f = showBinary . f . readBinary
    in 
    (wrap encode, wrap decode)

encode :: [Bool] -> [Bool]
encode xs = generator (codeLength xs) *. xs

decode :: [Bool] -> [Bool]
decode xs = let n = length xs
    in
    take (numDataBits n) $
    xs .+. correction n (parity n *. xs)

generator :: Int -> Matrix Bool
generator n = let a = core n 
    in
    identity (cols a) <-> a

parity :: Int -> Matrix Bool
parity n = let a = core n
    in
    a <|> identity (rows a)

correction :: Int -> [Bool] -> [Bool]
correction n syndrome =
    if all not syndrome then repeat False
    else
    let error = (syndrome==) <$> transpose (parity n)
    in
    if all not error then [] else error

core :: Int -> Matrix Bool
core n = 
    transpose .
    filter ((1/=).weight) .
    padBinary . map binary $ [1..n]

weight :: [Bool] -> Int
weight = length . filter id

numDataBits :: Int -> Int
numDataBits n = n - (ceiling . logBase 2 . fromIntegral $ n+1)

numParityBits :: Int -> Int
numParityBits k = until (\m -> numDataBits (m+k) >= k) (+1) 2 

codeLength :: [Bool] -> Int
codeLength = (\n -> n + numParityBits n) . length
