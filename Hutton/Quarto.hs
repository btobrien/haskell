

import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import System.IO
import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p <||> p' = (||) <$> p <*> p'

equal :: Eq a => (a,a) -> Bool
equal = uncurry (==)

neighbors :: [a] -> [(a,a)]
neighbors = zip <$> id <*> tail

--

size :: Int
size = 4

maxIndex :: Int
maxIndex = (size^2) - 1

type Piece = (Height, Color, Body, Shape)
data Height =   Tall | Short   deriving (Enum, Eq, Show)
data Color  =  White | Black   deriving (Enum, Eq, Show)
data Body   =  Solid | Hollow  deriving (Enum, Eq, Show)
data Shape  = Circle | Square  deriving (Enum, Eq, Show)
height (x,_,_,_) = x
color  (_,x,_,_) = x
body   (_,_,x,_) = x
shape  (_,_,_,x) = x

pieces :: [Piece]
pieces = (,,,) <$> [Tall,Short] <*> [White,Black] <*> [Solid,Hollow] <*> [Circle,Square]

piece :: Int -> Piece
piece = (pieces!!)

type Square = Maybe Piece
type Board = [[Square]] -- assumed to be size x size

newboard :: Board
newboard = (replicate size . replicate size) Nothing

--full :: Board -> Bool
--full = all isJust . concat

matchingOn :: Eq b => (Piece -> b) -> [Piece] -> Bool
matchingOn f = all equal . neighbors . map f 

matching :: [Piece] -> Bool
matching =
    matchingOn height <||>
    matchingOn color <||>
    matchingOn body <||>
    matchingOn shape

fullMatch :: [Square] -> Bool
fullMatch = fromMaybe False . fmap matching . sequence

connected :: Board -> Bool
connected board = check rows || check cols || check diags
    where 
        check = any fullMatch
        rows  = board
        cols  = transpose board
        diags = [diag board, (diag . reverse) board]
        diag b = [b !! i !! i | i <- [0..size-1]] 

type Position = (Board,Piece)
type Move = (Int,Piece)

won :: Position -> Bool
won = connected . fst

squaresLeft :: Board -> Int
squaresLeft = length . filter isNothing . concat

depth :: Board -> Int
depth = subtract (maxIndex + 1) . squaresLeft

valid :: Position -> Move -> Bool
valid (board,piece) (i,nextPiece) = let pieces = concat board in
    0 <= i && i <= maxIndex &&
    pieces !! i == Nothing &&
    (squaresLeft board == 1 || not (nextPiece == piece || elem (Just nextPiece) pieces))

update :: Piece -> Int -> Board -> Board
update piece index board = chunksOf size $ before ++ [Just piece] ++ after
    where (before, Nothing : after) = splitAt index . concat $ board

move :: Move -> Position -> Maybe Position
move (index, nextPiece) (board, piece) =
    if valid (board, piece) (index, nextPiece)
    then Just (update piece index board, nextPiece)
    else Nothing

-- use built in tree
data Tree a = Node a [Tree a]
    deriving Show

treeCount :: Tree a -> Int
treeCount (Node _ gs) = 1 + sum (map treeCount gs)

treeDepth :: Tree a -> Int
treeDepth (Node _ []) = 0
treeDepth (Node _ gs) = 1 + maximum (map treeDepth gs)

moves :: Position -> [Position]
moves pos 
    | won pos = []
    | otherwise = catMaybes . map (flip move pos) $ (,) <$> [0..maxIndex] <*> pieces

gametree :: Position -> Tree Position
gametree pos = Node pos $ map gametree (moves pos)

data Player = First | Tie | Second -- order determines utility
    deriving (Enum, Eq, Show, Ord)

resultOf :: Position -> Player
resultOf (board,_)
    | connected board = if (odd . depth) board then First else Second
    | otherwise = Tie

-- generalize mini max algo
type MiniMaxTree = Tree (Position,Player,Int)

minimax :: Tree Position -> MiniMaxTree
minimax (Node position []) = Node (position, resultOf position, 0) []
minimax (Node position ts) = Node (position, winner, mindepth + 1) ts'
        where
            preference = if (odd . depth . fst) position then minimum else maximum
            winner = preference [result | Node (_,result,_) _ <- ts']
            mindepth  = minimum [depth | Node (_,result,depth) _ <- ts', result == winner]
            ts' = map minimax ts

pickmove :: MiniMaxTree -> Position
pickmove (Node (_,best,depth) ts) = head [g' | Node (g',p',d') _ <- ts, p'==best, d'==(depth-1)]

finalresult :: MiniMaxTree -> Player
finalresult (Node (_,result,_) _) = result

bestmove :: Position -> Position
bestmove = pickmove . minimax . gametree

evaluate :: Position -> Player
evaluate = finalresult . minimax . gametree

wins :: Int -> Position -> [Board]
wins 1 = nub . map fst . filter won . moves
wins n = nub . map fst . filter (all (losing (n-1)) . moves) . moves  

winning :: Int -> Position -> Bool
winning n = not . null . wins n

losing :: Int -> Position -> Bool
losing n = all (any (winning n) . moves) . moves

losers :: Int -> Position -> [Position]
losers n = filter (any (winning n) . moves) . moves

iterativeDeepening pos = listToMaybe . concat . map (flip wins pos) $ [1..(squaresLeft (fst pos))]

showSquare :: Square -> String
showSquare Nothing = "----"
showSquare (Just (a,b,c,d)) = concat . map show $ [fromEnum a,fromEnum b,fromEnum c,fromEnum d]

showBoard :: Board -> String
showBoard = intercalate "\n\n" . map (intercalate " ") . (map . map) showSquare


















