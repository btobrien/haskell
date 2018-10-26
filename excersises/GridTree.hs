
module GridTree where
import Data.List

curry :: (a -> b -> c) -> (a,b) -> c
curry f (a,b) = f a b

type Tree a = [[a]]
type Height = Int
type Depth = Int
type Degree = Int
type Location = (Height, Depth)


valid :: Tree a -> Location -> Bool -- valid node, diff fn for valid tree
valid t (h,d) = length t > h && length (t!!h) > d

get :: Tree a -> Location -> a
get t (h,d) = t !! h !! d

line :: Tree a -> Location -> [a]
line t (h,_) = t !! h

hist :: Tree a -> Location -> [a]
hist t (h,d) = take d (t!!h)

path :: Tree a -> Location -> [a]
path t (h,d) = hist t (h,d+1) 

heightOf :: Location -> Height
heightOf = fst

depthOf :: Location -> Depth
depthOf = snd

next :: Eq a => Tree a -> Location -> Location
next t (h,d) = if length (t!!h) > d+1 then (h,d+1) else (h,d)

prev :: Location -> Location
prev (h,d) = if d > 0 then (h,d-1) else (h,0)

ancestor :: Eq a => Tree a -> Height -> Height -> Depth
ancestor t h h' = ((+(-1)) . length) (takeWhile (\(x,y) -> x==y) (zip (t!!h) (t!!h')))

fall :: Eq a => Tree a -> Location -> Location
fall t (h,d) = if length (t) > h+1 then (h+1,d') else (h,d)
	where d' = min d ((ancestor t h (h+1)) + 1)

climb :: Eq a => Tree a -> Location -> Location
climb t (h,d) = if h > 0 then (h-1,d') else (0,d)
	where d' = min d ((ancestor t h (h-1)) + 1)

sharePathThrough :: Eq a => Depth -> [a] -> [a] -> Bool
sharePathThrough d ln1 ln2 = (take (d+1) ln1) `isPrefixOf` ln2

areSiblings :: Eq a => Depth -> [a] -> [a] -> Bool
areSiblings d = sharePathThrough (d-1)

areNotSiblings :: Eq a => Depth -> [a] -> [a] -> Bool
areNotSiblings d ln1 ln2 = not (areSiblings d ln1 ln2)

highers :: Eq a => Tree a -> Location -> Tree a
highers t (h,d) = takeWhile (areNotSiblings d ln) t
	where ln = t !! h

siblings :: Eq a => Tree a -> Location -> Tree a
siblings t (h,d) = takeWhile (areSiblings d ln) (dropWhile (areNotSiblings d ln) t)
	where ln = t !! h

lowers :: Eq a => Tree a -> Location -> Tree a
lowers t (h,d) = dropWhile (areSiblings d ln) (dropWhile (areNotSiblings d ln) t)
	where ln = t !! h

groupSiblings :: Eq a => Tree a -> Location -> [Tree a]
groupSiblings t (h,d) = groupBy (sharePathThrough d) (siblings t (h,d)) 


add :: Tree a -> Location -> a -> (Tree a, Location)
add t (h,d) a | d + 1 == length (line t (h,d)) = 


{-|
chop :: Tree a -> Location -> (Tree a, Location)
rotateUp :: Tree a -> Location -> Location
rotateDown :: Tree a -> Location -> Location

snap :: Tree a -> Location -> Node
branch :: Tree a -> Location -> Node

add :: Tree a -> Location -> (Tree a, Location)
modify :: Tree a -> Location -> (Tree a, Location)
chop :: Tree a -> Location -> (Tree a, Location)
promote :: Tree a -> Location -> (Tree a, Location)

sub :: Tree a -> Location -> (Tree a, Location)
strip :: Tree a -> Location -> (Tree a, Location)
fold :: Tree a -> Location -> (Tree a, Location)
shearAbove :: Tree a -> Location -> (Tree a, Location)
shearBelow :: Tree a -> Location -> (Tree a, Location)
shave :: Tree a -> Location -> (Tree a, Location)
-}

while :: Eq a => (a -> a) -> a -> a
while f x = if (x == x') then x else while f x'
	where x' = f x

root :: Tree a -> Location -> Location
root _ (h,_) = (h,0)

leaf :: Eq a => Tree a -> Location -> Location
leaf t = while (next t)

top :: Eq a => Tree a -> Location -> Location
top t = while (climb t)

bottom :: Eq a => Tree a -> Location -> Location
bottom t = while (fall t)
