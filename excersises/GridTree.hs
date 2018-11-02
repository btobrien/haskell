
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

next :: Eq a => Tree a -> Location -> Location
next t (h,d) = if length (t!!h) > d+1 then (h,d+1) else (h,d)

prev :: Location -> Location
prev (h,d) = if d >= 0 then (h,d-1) else (h,0)

path :: Int -> [a] -> [a]
path = take . (+1)

ancestor :: Eq a => [a] -> [a] -> Int
ancestor xs ys = ((+(-1)) . length) (takeWhile (\(x,y) -> x==y) (zip xs ys))

sharePathThrough :: Eq a => Int -> [a] -> [a] -> Bool
sharePathThrough d ln1 ln2 = (path d ln1) `isPrefixOf` ln2

fall :: Eq a => Tree a -> Location -> Location
fall t (h,d) = if length (t) > h+1 then (h+1,d') else (h,d)
	where 
		d' = min d ((ancestor ln ln') + 1)
		ln  = t !! h
		ln' = t !! (h + 1)

climb :: Eq a => Tree a -> Location -> Location
climb t (h,d) = if h > 0 then (h-1,d') else (0,d)
	where 
		d' = min d ((ancestor ln ln') + 1)
		ln  = t !! h
		ln' = t !! (h - 1)

highers :: Eq a => Tree a -> Location -> Tree a
highers t (h,d) = takeWhile (not.sharePathThrough d ln) t
	where ln = t !! h

siblings :: Eq a => Tree a -> Location -> Tree a
siblings t (h,d) = takeWhile (sharePathThrough d ln) (dropWhile (not.sharePathThrough d ln) t)
	where ln = t !! h

lowers :: Eq a => Tree a -> Location -> Tree a
lowers t (h,d) = dropWhile (sharePathThrough d ln) (dropWhile (not.sharePathThrough d ln) t)
	where ln = t !! h

cousins :: Eq a => Tree a -> Location -> Tree a
cousins t (h,d) = siblings t (h,d-1)

groupCousinsBySibling :: Eq a => Tree a -> Location -> [Tree a]
groupCousinsBySibling t (h,d) = groupBy (sharePathThrough d) (cousins t (h,d)) 

isOnlyCousin :: Eq a => Tree a -> Location -> Bool
isOnlyCousin t loc = length (groupCousinsBySibling t loc) == 1

append :: Eq a => Tree a -> Height -> a -> Tree a
append t h a = take h t ++ [ln ++ [a]] ++ drop (h+1) t
	where ln = t !! h

height :: Eq a => Tree a -> [a] -> Height
height t ln = (length . takeWhile (not . isPrefixOf ln)) t

traverse :: Eq a => Tree a -> [a] -> Location
traverse t ln = (height t ln, length ln - 1)

pair :: a -> b -> (a,b)
pair a b = (a,b)

result :: Eq a => Tree a -> [a] -> (Tree a, Location)
result t ln = (t, traverse t ln)

snap :: Eq a => Tree a -> Location -> Location
snap t (h,d) = (h,d')
	where 
		ln = t !! h
		highs = highers t (h,d-1)
		lows = lowers t (h,d-1)
		a  = if null highs then [] else [ancestor ln (last highs) + 1]
		a'  = if null lows then [] else [ancestor ln (head lows) + 1]
		candidates = a ++ a'
		d' = if null candidates then d else maximum candidates 

branch :: Eq a => Tree a -> Location -> Location
branch t (h,d) = (h,d')
	where 
		ln = t !! h
		sibs = siblings t (h,d)
		a  = ancestor ln (head sibs) + 1
		a' = ancestor ln (last sibs) + 1
		d' = min a a'

rotateUp :: Eq a => Tree a -> Location -> Location
rotateUp t (h,d) = if (not.null) highs && areCousins then (h',d) else (h,d)
	where 
		ln = t !! h
		highs = highers t (h,d)
		candidate  = last highs
		areCousins = sharePathThrough (d-1) ln candidate
		h' = height t (path d candidate)

rotateDown :: Eq a => Tree a -> Location -> Location
rotateDown t (h,d) = if (not.null) lows && areCousins then (h',d) else (h,d)
	where 
		ln = t !! h
		lows = lowers t (h,d)
		candidate  = head lows
		areCousins = sharePathThrough (d-1) ln candidate
		h' = height t (path d candidate)

add :: Eq a => Tree a -> Location -> a -> (Tree a, Location)
add t (h,d) a | d' == length ln			= pair (append t h a) (h,d')
			  | otherwise				= result t' ln'
	where
		d' = d + 1
		ln = t !! h
		ln' = path d ln ++ [a]
		sibs = siblings t (h,d)
		t' = if any ((==a).(!!d')) sibs
				then t
				else highers t (h,d) ++ sibs ++ [ln'] ++ lowers t (h,d)

chop :: Eq a => Tree a -> Location -> (Tree a, Location)
chop t (h,d) = result t' ln'
	where
		ln = t !! h
		ln' = take d ln
		onlyCousin = isOnlyCousin t (h,d)
		t' = if (d==0 || not onlyCousin)
				then filter (not.sharePathThrough d ln) t
				else highers t (h,d) ++ [ln'] ++ lowers t (h,d)


promote :: Eq a => Tree a -> Location -> (Tree a, Location)
promote t (h,d) | h == 0    = pair t (h,d)
				| otherwise = result t' ln
	where 
		ln = t !! h
		a = ancestor (t!!h) (t!!(h-1))
		loc = (h,a+1)
		loc' = (h-1,a+1)
		t' = highers t loc' ++ siblings t loc ++ siblings t loc' ++ lowers t loc

demote :: Eq a => Tree a -> Location -> (Tree a, Location)
demote t (h,d) | h == length t = pair t (h,d)
			   | otherwise     = result t' ln
	where 
		ln = t !! h
		a = ancestor (t!!h) (t!!(h+1))
		loc = (h,a+1)
		loc' = (h+1,a+1)
		t' = highers t loc' ++ siblings t loc ++ siblings t loc' ++ lowers t loc

sub :: Eq a => Tree a -> Location -> (Tree a, Location)
sub t (h,d) = pair t' (h',0)
	where
		h' = h - length (highers t (h,d))
		t' = map (drop d) (siblings t (h,d))


{-|

modify :: Eq a => Tree a -> Location -> (Tree a, Location)

subclimb
subfall
subtop
subbottom

strip :: Eq a => Tree a -> Location -> (Tree a, Location)
shearAbove :: Eq a => Tree a -> Location -> (Tree a, Location)
shearBelow :: Eq a => Tree a -> Location -> (Tree a, Location)
shave :: Eq a => Tree a -> Location -> (Tree a, Location)
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
