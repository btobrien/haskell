

import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import System.IO
import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy


size :: Int
size = 4

data Height = Tall | Short
    deriving (Enum, Eq, Ord, Show)

data Color = White | Black
    deriving (Enum, Eq, Ord, Show)

data Body = Solid | Hollow
    deriving (Enum, Eq, Ord, Show)

data Shape = Circle | Square
    deriving (Enum, Eq, Ord, Show)

type Piece = (Height, Color, Body, Shape)
height (x,_,_,_) = x
color  (_,x,_,_) = x
body   (_,_,x,_) = x
shape  (_,_,_,x) = x

pieces :: [Piece]
pieces = (,,,) <$> [Tall,Short] <*> [White,Black] <*> [Solid,Hollow] <*> [Circle,Square]

type Square = Maybe Piece
type Board = [[Square]] -- required to be size x size

empty :: Board
empty = (replicate size . replicate size) Nothing

full :: Board -> Bool
full = all isJust . concat

diag :: Board -> [Square]
diag board = [board !! i !! i | i <- [0..size-1]] 

neighbors :: [a] -> [(a,a)]
neighbors xs = zip xs (tail xs)

matchingOn :: Eq b => (Piece -> b) -> [Piece] -> Bool
matchingOn f = all same . neighbors . map f 
    where same = uncurry (==)

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p <||> p' = (||) <$> p <*> p'

matching :: [Piece] -> Bool
matching =
    matchingOn height <||>
    matchingOn color <||>
    matchingOn body <||>
    matchingOn shape

matchingFull :: [Square] -> Bool
matchingFull = fromMaybe False . fmap matching . sequence

won :: Board -> Bool
won board = check rows || check cols || check diags
    where 
        check = any matchingFull
        rows  = board
        cols  = transpose board
        diags = [diag board, (diag . reverse) board]

done :: Board -> Bool
done  = won <||> full 

type Position = (Board,Piece)
type Move = (Int,Piece)

valid :: Position -> Move -> Bool
valid (board,piece) (i,nextPiece) =
    0 <= i && i < size^2 &&
    pieces !! i == Nothing &&
    nextPiece /= piece &&
    (not . elem (Just nextPiece)) pieces

    where pieces = concat board

