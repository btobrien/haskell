
module Data.Group where

import Data.Monoid
import Data.List hiding (cycle)
import Prelude hiding (cycle, (^))
import Control.Applicative
import Control.Monad
import Utils

import qualified Data.Set as Set
import Data.Set (Set, isSubsetOf)

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
selfinv = inverts <$> id <*> id

preserving :: Group a => [a] -> Bool
preserving xs = 
    xs == map (e<>) xs &&
    xs == map (<>e) xs

inverts :: Group a => a -> a -> Bool
inverts x y =
    e == x <> y &&
    e == y <> x

invertible :: Group a => [a] -> Bool
invertible xs =
    all (inverts <$> inv <*> id) xs
    && 
    xs =~= map inv xs

inverses :: Group a => [a] -> [[a]]
inverses = normalize . map (\x -> normalize [x,inv x]) . delete e

products :: Group a => [a] -> [a] -> [a]
products xs ys = normalize [x <> y | x <- xs, y <- ys]

closure :: Group a => [a] -> [a]
closure = products <$> id <*> id

closed :: Group a => [a] -> Bool
closed xs = closure xs =~= xs

uniqueness :: Group a => [a] -> Bool
uniqueness xs = length xs == length (normalize xs)

containsId :: Group a => [a] -> Bool
containsId = identity . head . sort

isGroup :: Group a => [a] -> Bool
isGroup = not.null
    <&&> containsId
    <&&> uniqueness
    <&&> preserving 
    <&&> invertible 
    <&&> closed

cayleyTable :: Group a => [a] -> [[a]]
cayleyTable xs = (<$>xs) . (<>) <$> xs
    
close :: Group a => [a] -> [a]
close = until closed closure 

gen :: Group a => [a] -> [a]
gen xs = close . (e:) $ xs ++ map inv xs

hasOrderOf :: Int -> [a] -> Bool
hasOrderOf n = (n==).length

cycle :: Group a => a -> [a] 
cycle g = generate (g<>) e 

cycles :: Group a => [a] -> [[a]] 
cycles = normalize . map (normalize.cycle)

order :: Group a => a -> Int 
order = length . cycle

generates :: Group a => [a] -> a -> Bool
generates xs x = length xs == length (cycle x)

generators :: Group a => [a] -> [a]
generators = filter <$> generates <*> id

nonGenerators :: Group a => [a] -> [a]
nonGenerators xs = filter (not . generates xs) xs

isCyclic :: Group a => [a] -> Bool
isCyclic = any <$> generates <*> id

cyclicSubgroups :: Group a => [a] -> [[a]]
cyclicSubgroups = cycles . delete e . nonGenerators

commute :: Group a => a -> a -> Bool
commute x y = x <> y == y <> x

commutes :: Group a => [a] -> a -> Bool
commutes xs x = all (commute x) xs

isAbelian :: Group a => [a] -> Bool
isAbelian = all (uncurry commute) . pairs

center :: Group a => [a] -> [a]
center = normalize . (filter <$> commutes <*> id)

conjugacy x = sort . map (\y -> inv x <> y <> x)
conjugacies g sg = normalize [conjugacy x sg | x <- g]

backProduct :: Group a => [a] -> [a]
backProduct = normalize . map (\(x,y) -> x <> inv y) . pairs

subgroupsWithOrder' :: Group a => (Int -> Bool) -> [a] -> [[a]]
subgroupsWithOrder' withOrder xs = do
    d <- filter withOrder . dividers $ length xs
    g <- filter isSubgroup . choose (d-1) . sort . delete e $ xs
    return (e:g)
    where
    isSubgroup g = 
        sort (map inv g) == g
        &&
        closure g `isSubsequenceOf` (e:g)

subgroups :: Group a => [a] -> [[a]]
subgroups = subgroupsWithOrder (const True)

subgroupsWithOrder :: Group a => (Int -> Bool) -> [a] -> [[a]]
subgroupsWithOrder withOrder xs =
    if isCyclic xs
    then filter (withOrder . length) (cyclicSubgroups xs) 
    else do
    d <- filter withOrder . dividers $ length xs
    g <- filter isSubgroup . pack (d-1) $ inverses xs
    return (e : concat g)
    where
    isSubgroup g = closure (map head g) `isSubsequenceOf` (e : sort (concat g))

instance (Group a, Group b) => Group (a,b) where
    inv (a,b) = (inv a, inv b)
instance (Group a, Group b, Group c) => Group (a,b,c) where
    inv (a,b,c) = (inv a, inv b, inv c)
instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
    inv (a,b,c,d) = (inv a, inv b, inv c, inv d)
