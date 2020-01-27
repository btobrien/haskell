
module Go where

import Control.Monad
import Control.Applicative

import Data.List (null)
import Plus (replaceAt, modifyAt)

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
size :: Board -> Size
size = ceiling . sqrt . fromIntegral . length

count :: Piece -> Board -> Int
count p = length . filter (==p) . pieces

start :: Int -> Board
start z = replicate (z*z) (annotated Empty)

place :: Piece -> Int -> Board -> Board
place p i xs | i < 0 = xs
place p i xs | i >= (size xs)^2 = xs
place p i xs = replaceAt i (annotated p) xs

annotate :: String -> Int -> Board -> Board
annotate str i xs | i < 0 = xs
annotate str i xs | i >= (size xs)^2 = xs
annotate str i xs = modifyAt i (setnote str) xs

clearnotes :: Board -> Board
clearnotes = map (setnote "")

squares :: Board -> [Annotated Square]
squares = zipWith (\location -> (label location <$>)) [0..]

--capture :: [Annotated Square] -> Board
--capture = map (fmap piece)

liberties :: Board -> Square -> Int
liberties xs y = 1
