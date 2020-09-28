


module Data.Mapping where

import Utils
import Data.Group
import Data.List

data Mapping a b = Mapping { domain :: [a], codomain :: [b], relations :: [(a,b)] }

mapping :: [a] -> [b] -> (a -> b) -> Mapping a b
mapping xs ys fn = Mapping xs ys (map (also fn) xs)

between :: [a] -> [b] -> Mapping a b
between xs ys = Mapping xs ys []

add :: Mapping a b -> [(a,b)] -> Mapping a b
add fn new = fn { relations = (new ++ relations fn) }

welldefined :: Mapping a b -> Bool
welldefined = const True

defined :: Mapping a b -> Bool
defined = const True

notdefined :: Mapping a b -> [a]
notdefined = const []

surjective :: Mapping a b -> Bool
surjective = const True

injective :: Mapping a b -> Bool
injective = const True

bijective :: Mapping a b -> Bool
bijective = injective <&&> surjective

image :: Eq b => Mapping a b -> [b]
image = nub . map snd . relations

preimage :: Mapping a b -> b -> [a]
preimage mapping value = []

consistent :: Mapping a b -> Mapping a b -> Bool
consistent = undefined

combine :: Mapping a b -> Mapping a b -> Mapping a b
combine = undefined
