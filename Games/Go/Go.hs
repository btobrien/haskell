
module Go where

import Control.Monad
import Control.Applicative

import Data.List
import Plus (replaceAt, modifyAt, (<&&>), composeWith)

data Piece = Black | Empty | White
    deriving (Show, Read, Enum, Eq, Ord)

type Coordinate = Int

type Square = (Coordinate, Piece); location = fst; piece = snd;

label :: Coordinate -> Piece -> Square
label = (,)

occupied :: Square -> Bool
occupied = (/=Empty) . piece 

type Annotated a = (String, a); value = snd; annotation = fst

annotated :: a -> Annotated a
annotated = (,) ""

setnote :: String -> Annotated a -> Annotated a
setnote str x = (str, value x)

isAnnotated :: Annotated a -> Bool
isAnnotated = null . annotation

type Board = [Annotated Piece]

pieces :: Board -> [Piece]
pieces = map value

type Size = Int
--size :: [a] -> Size
size = ceiling . sqrt . fromIntegral . length

count :: Piece -> Board -> Int
count p = length . filter (==p) . pieces

start :: Int -> Board
start z = replicate (z*z) (annotated Empty)

squares :: Board -> [Annotated Square]
squares = zipWith (fmap . label) [0..]

inbounds :: Size -> Int -> Bool
inbounds z i = i > 0 && i < z^2

place :: Piece -> Int -> Board -> Board
place p i xs | not (inbounds (size xs) i) = xs
place p i xs = captureAround i . replaceAt i (annotated p) $ xs

annotate :: String -> Int -> Board -> Board
annotate str i xs | not (inbounds (size xs) i) = xs
annotate str i xs = modifyAt i (setnote str) xs

clearnotes :: Board -> Board
clearnotes = map (setnote "")

captureAround :: Int -> Board -> Board
captureAround i board = composeWith capture (adjacents (size board) i ++ [i]) board

vacants :: [Piece] -> [Coordinate]
vacants = map location . filter (not . occupied) . zip [0..]

capture :: Int -> Board -> Board
capture i board = if (pieces board !! i) == Empty then board else
	let
	xs = pieces board
	captured = prisoners (adjacencies (size board)) (friends xs) (vacants xs) (frontierFrom i)
	in
	composeWith (place Empty) captured board

adjacents :: Size -> Coordinate -> [Coordinate]
adjacents z i = filter (inbounds z <&&> inline z i) [i+1, i-1, i+z, i-z]
	where
	inline z x y = let
		col = (`mod`z)
		row = (`div`z)
		in
		row x == row y || col x == col y

adjacent :: Size -> Coordinate -> Coordinate -> Bool
adjacent z x y = y `elem` adjacents z x

adjacencies :: Size -> [Coordinate] -> [Coordinate] -> [Coordinate]
adjacencies z xs ys = concatMap (adjacents z) xs `intersect` ys

areFriends :: Size -> Square -> Square -> Bool
areFriends z x y = piece x == piece y && adjacent z (location x) (location y)

friends :: [Piece] -> Coordinate -> [Coordinate]
friends pieces i = let p = (i, pieces !! i)
	in
	map location . filter (areFriends (size pieces) p) . zip [0..] $ pieces

prisoners :: Eq a => Connector a -> Expander a -> [a] -> Frontier a -> [a]
prisoners connector expander escapes soldiers =
	if null (frontier soldiers)
		then visited soldiers
	else if not . null $ connector (frontier soldiers) escapes
		then []
	else
		prisoners connector expander escapes (expand expander soldiers)
	
type Frontier a = ([a],[a]); visited = fst; frontier = snd
type Connector a = [a] -> [a] -> [a]
type Expander a = a -> [a]

frontierFrom :: a -> Frontier a
frontierFrom x = ([],[x])

expand :: Eq a => Expander a -> Frontier a -> Frontier a
expand expander (vs,fs) = let vs' = vs ++ fs in (vs', concatMap expander fs \\ vs')


