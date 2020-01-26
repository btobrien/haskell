
module Hanoi where

import Data.Maybe
import Control.Applicative

type Disk = Int
data Board = Board { left :: [Disk], center :: [Disk], right :: [Disk] }
    deriving (Eq, Show, Read)

start :: Int -> Board
start n = Board [1..n] [] []

won :: Board -> Bool
won = (&&) <$> null.left <*> null.center

update :: Int -> ([Disk] -> [Disk]) -> Board -> Maybe Board
update 0 modify board = Just $ board { left = modify (left board) }
update 1 modify board = Just $ board { center = modify (center board) }
update 2 modify board = Just $ board { right = modify (right board) }
update _ _ _ = Nothing

safehead :: [a] -> Maybe a
safehead = listToMaybe . take 1

top :: Int -> Board -> Maybe Disk
top 0 = safehead.left
top 1 = safehead.center
top 2 = safehead.right
top _ = const Nothing

push :: Int -> Disk -> Board -> Maybe Board
push index disk board = if disk < fromMaybe maxBound (top index board)
    then update index (disk:) board
    else Nothing

pop :: Int -> Board -> Maybe (Disk,Board)
pop index board = (,) <$> top index board <*> update index (drop 1) board

move :: (Int,Int) -> Board -> Board
move (from,to) board = fromMaybe board $ pop from board >>= uncurry (push to)
