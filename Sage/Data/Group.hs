
module Data.Group where

import Data.Monoid
import Data.List
import Control.Applicative
import Control.Monad
import Utils

import qualified Data.Map as Map
import Data.Map (Map)

infixl 6 .>
(.>) :: Monoid a => a -> a -> a
(.>) = flip (<>)

class (Monoid a, Ord a) => Group a where
    inv :: a -> a
    e :: a
    e = mempty

identity :: Group a => a -> Bool
identity = (==e)

preserving :: Group a => [a] -> Bool
preserving xs = 
    xs == map (e<>) xs &&
    xs == map (<>e) xs

inverses :: Group a => a -> a -> Bool
inverses x y =
    e == x <> y &&
    e == y <> x

invertible :: Group a => [a] -> Bool
invertible xs =
    all (inverses <$> inv <*> id) xs
    && 
    xs =~= map inv xs

products :: Group a => [a] -> [a]
products = nub . map (uncurry (<>)) . pairs

closed :: Group a => [a] -> Bool
closed xs = products xs =~= xs

uniqueness :: Group a => [a] -> Bool
uniqueness xs = length xs == length (nub xs)

isGroup :: Group a => [a] -> Bool
isGroup = not.null
    <&&> uniqueness
    <&&> preserving 
    <&&> invertible 
    <&&> closed

cayleyTable :: Group a => [a] -> [[a]]
cayleyTable xs = (<$>xs) . (<>) <$> xs
	
close :: Group a => [a] -> [a]
close = until closed products 

genFrom :: Group a => [a] -> [a]
genFrom xs = close . nub $ xs ++ map inv xs

isGenOf :: Group a => [a] -> a -> Bool
isGenOf xs x = length xs == length (gen x)

generators :: Group a => [a] -> [a]
generators = filter <$> isGenOf <*> id

isCyclic :: Group a => [a] -> Bool
isCyclic = any <$> isGenOf <*> id

commute :: Group a => a -> a -> Bool
commute x y = x <> y == y <> x

commutes :: Group a => [a] -> a -> Bool
commutes xs x = all (commute x) xs

commuters :: Group a => [a] -> [a]
commuters = filter <$> commutes <*> id

selfInvs :: Group a => [a] -> [a]
selfInvs = filter (inverses <$> id <*> id)

isAbelian :: Group a => [a] -> Bool
isAbelian = all (uncurry commute) . pairs

gen :: Group a => a -> [a] 
gen g = generate (g<>) e 

order :: Group a => a -> Int 
order = length . gen

backProduct :: Group a => [a] -> [a]
backProduct = map (\(x,y) -> x <> inv y) . pairs

isSubgroup :: Group a => [a] -> Bool
isSubgroup xs = xs =~= backProduct xs

subgroupsOf :: Group a => Int -> [a] -> [[a]]
subgroupsOf n xs =
    [ (e:g) | g <- chooseFrom (delete e xs) (n-1)
    , isSubgroup (e:g) ]

subgroups :: Group a => [a] -> [[a]]
subgroups xs = let n = length xs in
    [ (e:g) | d <- dividers n
    , g <- chooseFrom (delete e xs) (d-1)
    , isSubgroup (e:g) ]

instance (Group a, Group b) => Group (a,b) where
    inv (a,b) = (inv a, inv b)
instance (Group a, Group b, Group c) => Group (a,b,c) where
    inv (a,b,c) = (inv a, inv b, inv c)
instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
    inv (a,b,c,d) = (inv a, inv b, inv c, inv d)
