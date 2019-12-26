
module Data.Group.Tile.Solver where 

import Data.List
import Data.Maybe
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

inverse :: Move -> Move
inverse U = D
inverse D = U
inverse L = R
inverse R = L

areInverses :: (Move,Move) -> Bool
areInverses = equal . fmap inverse

reduce :: [Move] -> [Move]
reduce = until reduced reduce'
    where
    reduce' [] = []
    reduce' [x] = [x]
    reduce' (x:y:zs) = if areInverses (x,y)
        then reduce zs
        else x : reduce (y:zs)

reduced :: [Move] -> Bool
reduced = all (not . areInverses) . neighbors

reflect :: Board -> Board
reflect = transpose . (map . map) swap

shouldReflect :: Board -> Bool
shouldReflect = (>) <$> width <*> height

done :: Board -> Bool
done = ((2,2)==) . size

trim :: Board -> Board
trim = init

hint :: Board -> Maybe Move
hint = listToMaybe . solution

solution :: Board -> [Move]
solution = reduce . evalState solve

solve :: State Board [Move]
solve = get >>= \board ->
    if done board then
        return []
    else if shouldReflect board then
        map reflected <$> (modify reflect >> solve)
    else
        solveEdge <++> (modify trim >> solve)
    
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
        else mapM move (align ++ reach)

escort :: Tile -> State Board [Move]
escort x = get >>= \board ->
    let
    (v,u) = difference x (location x board)
    step =
        if v == 0 then fall x
        else if u == 1 && v < 0 then swoop
        else if u == 0 then sweep
        else if v < 0 then zig else zag
    in
    if (v,u) == (0,0) then return []
    else if fst x == 0 && (v,u) == (0,1) then wax
    else step <++> escort x
    
move :: Move -> State Board Move
move m = modify (slide m) >> return m

wax :: State Board [Move]
wax = mapM move [L,U,R,D,L,U,R,U,L,D,D,R,U,L,U,R,D]

zig :: State Board [Move]
zig = mapM move [R,U,L,U,R,D]
    
zag :: State Board [Move]
zag = mapM move [L,U,R,U,L,D]

sweep :: State Board [Move]
sweep = mapM move [L,U,R,D,L]

swoop :: State Board [Move]
swoop = mapM move [R,U,L,D,R] 

fall :: Tile -> State Board [Move]
fall x = if fst x == 0
    then mapM move [L,U,U,R,D] 
    else mapM move [R,U,U,L,D]

