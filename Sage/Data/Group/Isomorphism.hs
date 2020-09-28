

module Data.Group.Isomorphism where

import Utils
import Data.List

import Data.Group
import Data.Mapping
import Data.Group.Permutation

preserving :: (Group a, Group b) => (a -> b) -> (a,a) -> Bool
preserving iso (x,y) = iso (x<>y) == (iso x)<>(iso y)

homomorphic :: (Group a, Group b) => [a] -> (a -> b) -> Bool
homomorphic xs iso = all (preserving iso) (pairs xs)

isomorphic :: (Group a, Group b) => [a] -> (a -> b) -> [b] -> Bool
isomorphic xs iso ys = 
    map iso xs =~= ys &&
    homomorphic xs iso

automorphisms :: Group a => [a] -> [Permutation a]
automorphisms = undefined


isomorphisms :: (Group a, Group b) => [[[a]]] -> [[[b]]] -> Mapping a b -> [Mapping a b]
isomorphisms acycles bcycyles mapping = undefined

cayleyTree :: Group a => [a] -> [[a]]
cayleyTree gs = convergenceOn (map productOf) . iterate (flip (foldl expand) gs) $ [[]]

expand :: Group a => [[a]] -> a -> [[a]]
expand xs g = normalizeOn productOf $
    xs ++ map (g:) xs


-- prove that a mapping that preserves composition between generators must be homomorphic
