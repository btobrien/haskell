
module Data.Group where

import Data.Monoid
import Data.List hiding (cycle)
import Prelude hiding (cycle, (^))
import Control.Applicative
import Control.Monad
import Utils

class (Monoid a, Ord a) => Group a where
    inv :: a -> a
    e :: a
    e = mempty

(><) :: Group a => a -> a -> a
(><) = flip (<>)

fold :: Group a => [a] -> a
fold = foldl' (><) e

infixr 9 ^
g ^ 0 = e
g ^ 1 = g
g ^ n | n < 0 = inv g ^ abs n
g ^ n = let m = n `div` 2 in g^m <> g^(n-m)

cayleyTable :: Group a => [a] -> [[a]]
cayleyTable xs = (<$>xs) . (<>) <$> xs
    
identity :: Group a => a -> Bool
identity = (==e)

selfinv :: Group a => a -> Bool
selfinv = inverts <$> id <*> id

preserving :: Group a => [a] -> Bool
preserving xs = 
    xs == map (e<>) xs &&
    xs == map (<>e) xs

inverts :: Group a => a -> a -> Bool
inverts x y = inv x == y

invertible :: Group a => [a] -> Bool
invertible xs =
    all (inverts' <$> inv <*> id) xs
    && 
    xs =~= map inv xs
    where
    inverts' x y =
        e == x <> y &&
        e == y <> x


inverses :: Group a => [a] -> [[a]]
inverses = normalize . map (\x -> normalize [x,inv x]) . delete e

products :: Group a => [a] -> [a] -> [a]
products xs ys = normalize [x <> y | x <- xs, y <- ys]

closure :: Group a => [a] -> [a]
closure = products <$> id <*> id

closed :: Group a => [a] -> Bool
closed xs = closure xs <~ (e : sort xs)

close :: Group a => [a] -> [a]
close = until closed closure . normalize 

closedOrder :: Group a => [a] -> Int
closedOrder xs = convergence . map length . iterate closure $ e : xs ++ map inv xs

gen :: Group a => [a] -> [a]
gen xs = close . (e:) $ xs ++ map inv xs

gens :: Group a => [a] -> [a] -> Bool
gens gs hs = closedOrder hs == length gs

minGen :: Group a => [a] -> [a]
minGen gs = head .
    filter (gens gs) .
    powerset' .
    reverse $
    delete e gs

decompose :: Group a => [a] -> a -> [(Int,a)]
decompose = undefined

uniqueness :: Group a => [a] -> Bool
uniqueness xs = length xs == length (normalize xs)

-- requires identity to be the minimal element
checkid :: Group a => [a] -> Bool
checkid = identity . head . sort

isGroup :: Group a => [a] -> Bool
isGroup = not.null
    <&&> checkid
    <&&> uniqueness
    <&&> preserving 
    <&&> invertible 
    <&&> closed

cycle :: Group a => a -> [a] 
cycle g = generate (g<>) e 

generates :: Group a => [a] -> a -> Bool
generates xs x = length xs == length (cycle x)

generators :: Group a => [a] -> [a]
generators = filter <$> generates <*> id

nonGenerators :: Group a => [a] -> [a]
nonGenerators xs = filter (not . generates xs) xs

--should be sorted by order??
cycles :: Group a => [a] -> [[a]] 
cycles =
    normalize .
    map (normalize . delete e . cycle) .
    delete e .
    nonGenerators

subgroupsWithOrder :: Group a => (Int -> Bool) -> [a] -> [[a]]
subgroupsWithOrder withOrder xs = do
    d <- filter withOrder $ divisors (length xs)
    g <- filter closed $ pack (d-1) (cycles xs)
    return (e:g)

subgroups :: Group a => [a] -> [[a]]
subgroups xs = let n = largestDivisor (length xs) in subgroupsWithOrder(<n) xs +|+ subgroupsWithOrder(==n) xs

setsFrom :: Group a => (a -> [a] -> [a]) -> [a] -> [a] -> [[a]] 
setsFrom f hs gs = normalize [ sort (f g hs) | g <- gs]

conjugate :: Group a => a -> a -> a
conjugate x y = inv x <> y <> x

cosetsr :: Group a => [a] -> [a] -> [[a]] 
cosetsr = setsFrom (\g -> map (<>g))

cosetsl :: Group a => [a] -> [a] -> [[a]] 
cosetsl = setsFrom (\g -> map (g<>))

cosets :: Group a => [a] -> [a] -> [[a]] 
cosets = cosetsl

matchingCosets :: Group a => [a] -> [a] -> Bool
matchingCosets hs gs = cosetsr hs gs == cosetsl hs gs

conjugacies :: Group a => [a] -> [a] -> [[a]]
conjugacies = setsFrom (\g -> map (conjugate g))

backProduct :: Group a => [a] -> [a]
backProduct = normalize . map (\(x,y) -> x <> inv y) . pairs

isCyclic :: Group a => [a] -> Bool
isCyclic = any <$> generates <*> id

order :: Group a => a -> Int 
order = length . cycle

hasOrderOf :: Int -> [a] -> Bool
hasOrderOf n = (n==).length

commute :: Group a => a -> a -> Bool
commute x y = x <> y == y <> x

isAbelian :: Group a => [a] -> Bool
isAbelian = all (uncurry commute) . pairs

commutes :: Group a => [a] -> a -> Bool
commutes xs x = all (commute x) xs

center :: Group a => [a] -> [a]
center = normalize . (filter <$> commutes <*> id)

instance (Group a, Group b) => Group (a,b) where
    inv (a,b) = (inv a, inv b)
instance (Group a, Group b, Group c) => Group (a,b,c) where
    inv (a,b,c) = (inv a, inv b, inv c)
instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
    inv (a,b,c,d) = (inv a, inv b, inv c, inv d)
