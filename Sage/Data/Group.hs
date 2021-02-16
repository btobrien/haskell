
module Data.Group where

import Data.Monoid
import Data.List hiding (cycle)
import Prelude hiding (cycle)
import Control.Applicative
import Control.Monad
import Utils
import Data.Prime

cycle :: (Eq a, Monoid a) => a -> [a] 
cycle g = generate (g<>) mempty 

class (Monoid a, Ord a) => Group a where
    inverse :: a -> a
    inverse = last . cycle -- obviously not efficient & only works if finite
    identity :: a
    identity = mempty

productOf :: Group a => [a] -> a
productOf = foldl' (flip (<>)) identity

square :: Monoid a => a -> a
square x = x <> x

power :: (Integral n, Group a) => n -> a -> a
power exponent | exponent < 0 = power (abs exponent) . inverse
power exponent = productOf . zipFilter (binary exponent) . iterate square
-- implements repeated squares

infixr 9 .^
(.^) :: (Integral n, Group a) => a -> n -> a
(.^) = flip power

cayleyTable :: Group a => [a] -> [[a]]
cayleyTable xs = (<$>xs) . (<>) <$> xs
    
selfinv :: Group a => a -> Bool
selfinv = inverts <$> id <*> id

identifiable :: Group a => [a] -> Bool
identifiable xs = 
    xs == map (identity<>) xs &&
    xs == map (<>identity) xs

inverts :: Group a => a -> a -> Bool
inverts x y = inverse x == y

invertible :: Group a => [a] -> Bool
invertible xs =
    all (inverts' <$> inverse <*> id) xs
    && 
    xs =~= map inverse xs
    where
    inverts' x y =
        identity == x <> y &&
        identity == y <> x

inverses :: Group a => [a] -> [[a]]
inverses = normalize . map (\x -> normalize [x,inverse x]) . delete identity

products :: Group a => [a] -> [a] -> [a]
products xs ys = normalize [x <> y | x <- xs, y <- ys]

closure :: Group a => [a] -> [a]
closure = products <$> id <*> id

closed :: Group a => [a] -> Bool
closed xs = closure xs <~ (identity : sort xs)

close :: Group a => [a] -> [a]
close = until closed closure . normalize 

gen :: Group a => [a] -> [a]
gen xs = close . (identity:) $ xs ++ map inverse xs

gens :: Group a => [a] -> [a] -> Bool
gens gs hs = length (gen hs) == length gs

-- sort on order to prefer "smaller" generators
-- so cayley diagrams simpler
minGen :: Group a => [a] -> [a]
minGen gs = head .
    filter (gens gs) .
    choices .
    sortOn order $
    delete identity gs

decompose :: Group a => [a] -> a -> [(Int,a)]
decompose = undefined

uniqueness :: Group a => [a] -> Bool
uniqueness xs = length xs == length (normalize xs)

-- checks whether identity is the minimal element
checkid :: Group a => [a] -> Bool
checkid = (==identity) . head . sort

isGroup :: Group a => [a] -> Bool
isGroup = not.null
    <&&> checkid
    <&&> uniqueness
    <&&> identifiable 
    <&&> invertible 
    <&&> closed

generates :: Group a => [a] -> a -> Bool
generates xs x = length xs == length (cycle x)

generators :: Group a => [a] -> [a]
generators = filter <$> generates <*> id

nonGenerators :: Group a => [a] -> [a]
nonGenerators xs = filter (not . generates xs) xs

-- includes non-primitive cycles
cycles :: Group a => [a] -> [[a]] 
cycles =
    sortOn length .
    normalize .
    map (normalize . delete identity . cycle) .
    delete identity .
    nonGenerators

primitiveCycles :: Group a => [a] -> [[a]] 
primitiveCycles xs = let cs = cycles xs in
    reverse $ filter (not . (\x -> any (isSubsequenceOf x) (delete x cs))) cs 

subgroupsWithOrder :: Group a => (Int -> Bool) -> [a] -> [[a]]
subgroupsWithOrder withOrder xs = do
    d <- filter withOrder . map fromInteger $ divisors (toInteger . length $ xs)
    g <- filter closed $ pack (d-1) (cycles xs)
    return (identity:g)

subgroups :: Group a => [a] -> [[a]]
subgroups xs = let n = fromInteger $ largestDivisor (toInteger(length xs)) in subgroupsWithOrder(<n) xs +|+ subgroupsWithOrder(==n) xs

setsFrom :: Group a => (a -> [a] -> [a]) -> [a] -> [a] -> [[a]] 
setsFrom f hs gs = normalize [ sort (f g hs) | g <- gs]

conjugate :: Group a => a -> a -> a
conjugate x y = inverse x <> y <> x

rcosets :: Group a => [a] -> [a] -> [[a]] 
rcosets = setsFrom (\g -> map (<>g))

lcosets :: Group a => [a] -> [a] -> [[a]] 
lcosets = setsFrom (\g -> map (g<>))

cosets :: Group a => [a] -> [a] -> [[a]] 
cosets = lcosets

normal :: Group a => [a] -> [a] -> Bool
normal hs gs = rcosets hs gs == lcosets hs gs

conjugacies :: Group a => [a] -> [a] -> [[a]]
conjugacies = setsFrom (\g -> map (conjugate g))

backProduct :: Group a => [a] -> [a]
backProduct = normalize . map (\(x,y) -> x <> inverse y) . pairs

isCyclic :: Group a => [a] -> Bool
isCyclic = any <$> generates <*> id

order :: Group a => a -> Int 
order = length . cycle

--hasOrderOf :: Int -> [a] -> Bool
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
    inverse (a,b) = (inverse a, inverse b)
instance (Group a, Group b, Group c) => Group (a,b,c) where
    inverse (a,b,c) = (inverse a, inverse b, inverse c)
instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
    inverse (a,b,c,d) = (inverse a, inverse b, inverse c, inverse d)
