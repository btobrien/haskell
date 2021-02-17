
module Data.Algabraic.Code.Repetition where

import Data.List (sortOn, sort, group)

import Utils (chunksOf, (.:))
import Data.Algabraic.Code (Code)

repetition :: Int -> Code
repetition n = (encode n, decode n)

encode :: Int -> String -> String
encode = concatMap . replicate

decode :: Int -> String -> String
decode = map mostFrequent .: chunksOf
    where
    mostFrequent =
        head . head .
        sortOn (negate . length) .
        group . sort
