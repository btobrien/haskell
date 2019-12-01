
module Data.Group where

import Data.Monoid
import Data.List hiding (cycle)
import Prelude hiding (cycle, (^))
import Control.Applicative
import Control.Monad
import Utils

import qualified Data.Map as Map
import Data.Map (Map)

class (Monoid a, Ord a) => Group a where
    inv :: a -> a
    e :: a
    e = mempty

infixr 9 ^
g ^ 0 = e
g ^ 1 = g
g ^ n | n < 0 = inv g ^ abs n
g ^ n = (g <> g) ^ (n - 1)
 -- could use binary evaluation here..

identity :: Group a => a -> Bool
identity = (==e)

selfinv :: Group a => a -> Bool
selfinv = inverses <$> id <*> id

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

gen :: Group a => [a] -> [a]
gen xs = close . nub . (e:) $ xs ++ map inv xs

cycle :: Group a => a -> [a] 
cycle g = generate (g<>) e 

order :: Group a => a -> Int 
order = length . cycle

generates :: Group a => [a] -> a -> Bool
generates xs x = length xs == length (cycle x)

generators :: Group a => [a] -> [a]
generators = filter <$> generates <*> id

isCyclic :: Group a => [a] -> Bool
isCyclic = any <$> generates <*> id

commute :: Group a => a -> a -> Bool
commute x y = x <> y == y <> x

commutes :: Group a => [a] -> a -> Bool
commutes xs x = all (commute x) xs

center :: Group a => [a] -> [a]
center = filter <$> commutes <*> id

isAbelian :: Group a => [a] -> Bool
isAbelian = all (uncurry commute) . pairs

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
