module Data.Group.Action where

import Data.Group
import Data.Maybe
import Data.List 
import Utils

equivalent :: (Group a, Eq b) => [a] -> (a -> b -> b) -> b -> b -> Bool
equivalent gs action x y = isJust . find (==y) $ map (flip action x) gs

orbits :: (Group a, Eq b) => [a] -> (a -> b -> b) -> [b] -> [[b]]
orbits = classes .: equivalent

fixedPointsOf :: (Group a, Eq b) => (a -> b -> b) -> a -> [b] -> [b]
fixedPointsOf action g = filter $ action g <==> id

fixedPoints :: (Group a, Eq b) => (a -> b -> b) -> [a] -> [b] -> [(a,[b])]
fixedPoints action gs xs = mapAlso (flip (fixedPointsOf action) xs) gs

-- aka, isotropy subgroup of x
stabilizerOf :: (Group a, Eq b) => (a -> b -> b) -> b -> [a] -> [a]
stabilizerOf action x = filter $ \g -> action g x == x

stabilizers :: (Group a, Eq b) => (a -> b -> b) -> [a] -> [b] -> [(b,[a])]
stabilizers action gs xs = mapAlso (flip (stabilizerOf action) gs) xs
