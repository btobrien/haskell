

-- theory: small bite is always losing
-- theory: winning one bite more than half-height (after 3 1 1)

module Theory where

import Data.List
import Data.Bool (bool)

import Plus
import Chomp

isRectangle :: Bar -> Bool
isRectangle = and . (zipWith (==) <*> tail)

isOneBite :: Bar -> Bool
isOneBite = (2==) . length . group 

isBed :: Bar -> Bool
isBed = isOneBite <&&> (length.head <>> length.head.tail) . group

isChair :: Bar -> Bool
isChair = isOneBite <&&> (length.head <<> length.head.tail) . group

-- conjecture: all rectangles have a single solution (chair/bed)
    -- It is conjectured that sequences (chair-heights) and (bed-heights) are complementary.
    -- In other words, that the winning move in (p,p,p) is unique.
    -- Each number below 130000 belongs to precisely one. 
    -- note: 8x10 chomp is the smallest (known?) example with multiple winning moves
oneBiteTheory :: [Bar] -> Bool
oneBiteTheory = all ((1==).length) . groupOn head . filter isOneBite 

-- conjecture: bite heights for beds/chairs are, respectively, monotonically increasing
biteHeight :: Bar -> Int
biteHeight = head <-> last

-- theory: a two-step is losing iff it's balanced!!!!

isStep :: Int -> Bar -> Bool
isStep n = const n <==> last

-- which steps have inductive patterns, and which don't???
    -- answer: bed steps are the only steps to have an inductive pattern
    
-- seems like the 2 2 1, inductive layer is very important

isBalanced :: Bar -> Bool 
isBalanced = head <==> sum.tail

-- chair theorem: either a chair is losing, or there is a winning move that is also a chair
--
-- every time there are consecutive chairs, the chairs lose a balance point

loserSet = subLosers $ replicate 3 75

showBar :: Bar -> String
showBar = unwords . map (\x -> if x < 10 then show x ++ " " else show x)

showBars :: [Bar] -> String
showBars = unlines . map showBar

putBars = putStr . showBars

writeBars file = writeFile file . showBars

-- the index of the bed step determines the stable imbalance of losers in the limit
--
--
-- Let f(q,r) be the unique p such that (p,q,r) is a P-position. (Here (p,q,r) stands for (p,min(p,q),min(p,q,r)).) There is a simple recurrence for f(q,r):
--
-- If r > q, then f(q,r) = f(q,q).
-- (That is, f is constant on verticals above the diagonal.)
-- Otherwise, if f(q-1,r) < q, then f(q,r) = f(q-1,r).
-- (That is, if f(q,r) = q then f remains constant on the rest of this row.)
-- If neither case occurs, then f(q,r) is the smallest positive integer not among f(a,r)
    -- for a < q or among f(q,b) for b < r. 
--
--
