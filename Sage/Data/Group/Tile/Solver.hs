
module Data.Group.Tile.Solver where 

import Data.List
import Control.Applicative
import Control.Monad.State.Lazy

import Utils
import Data.Group.Tile

data Move = U | D | L | R deriving (Enum, Eq, Ord, Show, Read)

slide :: Move -> Board -> Board
slide U = up
slide D = down
slide L = left
slide R = right

reflected :: Move -> Move
reflected U = L
reflected L = U
reflected D = R
reflected R = D

reflect :: Board -> Board
reflect = transpose . (map.map) swap

shouldReflect :: Board -> Bool
shouldReflect = (>) <$> width <*> height

done :: Board -> Bool
done = ((2,2)==) . size

solve :: State Board [Move]
solve = get >>= \board ->
    if done board then
        return []
    else if shouldReflect board then
        map reflected <$> (modify reflect >> solve)
    else
        solveEdge <++> (modify init >> solve)
    
solveEdge :: State Board [Move]
solveEdge = gets corner >>= solveEdges
    where
    solveEdges x =
        if fst x < 0 then return []
        else solveTile x <++> solveEdges (shift (-1,0) x)

solveTile :: Tile -> State Board [Move]
solveTile x = get >>= \board ->
    if board `correctAt` x then return []
    else approach x <++> escort x

approach :: Tile -> State Board [Move]
approach x = get >>= \board ->
    let
    c = location cursor board
    (v,u) = difference (location x board) c
    align =
        if v == 0 then []
        else if v > 0 then replicate v L
        else replicate (abs v) R
    reach =
        if u > 0 then replicate (u-1) U
        else replicate (abs u) D
    adjust = if snd c == 0 then move U else move D
    in
    if u == 0 || snd c == height board - 1
        then adjust <:> approach x
        else mapM move (align++reach)

-- assumes to start from above
escort :: Tile -> State Board [Move]
escort x = get >>= \board ->
    let (v,u) = difference x (location x board)
    in
    if (v,u) == (0,0) then return []
    else if fst x == 0 && (v,u) == (0,1) then wax
    else if v == 0 then (if fst x == 0 then fade else fall) <++> escort x
    else if u == 1 && v < 0 then swoop <++> escort x
    else if u == 0 then sweep <++> escort x
    else zigzag v <++> escort x
    
wax :: State Board [Move]
wax = mapM move [L,U,R,D,L,U,R,U,L,D,D,R,U,L,U,R,D]

zigzag :: Int -> State Board [Move]
zigzag v = if v < 0
    then mapM move [R,U,L,U,R,D]
    else mapM move [L,U,R,U,L,D]

sweep :: State Board [Move]
sweep = mapM move [L,U,R,D,L]

swoop :: State Board [Move]
swoop = mapM move [R,U,L,D,R] 

fall :: State Board [Move]
fall = mapM move [R,U,U,L,D]

fade :: State Board [Move]
fade = mapM move [L,U,U,R,D]

move :: Move -> State Board Move
move m = modify (slide m) >> return m


