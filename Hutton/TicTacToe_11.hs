
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import System.IO
import Control.Monad
import Control.Monad.State.Lazy


size :: Int
size = 3

data Player = O | B | X
    deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

first :: Player
first = O

second :: Player
second = next first

empty :: Grid
empty = (replicate size . replicate size) B

full :: Grid -> Bool
full = all (/=B) . concat

turn :: Grid -> Player
turn g = if fs <= ss then first else second
    where
        ps = concat g
        fs = length . filter (==first) $ ps
        ss = length . filter (==second) $ ps

diag :: Grid -> [Player]
diag g = [g !! i !! i | i <- [0..size-1]] 

wins :: Player -> Grid -> Bool
wins p g = check rows || check cols || check diags
    where 
        check = any $ all (==p)
        rows  = g
        cols  = transpose g
        diags = [diag g, (diag.reverse) g]

won :: Grid -> Bool
won g = wins O g || wins X g

winner :: Grid -> Player
winner g 
    | wins O g  = O
    | wins X g  = X
    | otherwise = B

done :: Grid -> Bool
done g = won g || full g

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Player -> Grid -> Int -> Maybe Grid
move p g i = if valid g i then Just (chunksOf size $ xs ++ [p] ++ ys) else Nothing
    where (xs,B:ys) = splitAt i . concat $ g

data Tree a = Node a [Tree a]
    deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p 
    | done g     = []
    | otherwise = catMaybes [move p g i | i <- [0..((size^2)-1)]]

treeCount :: Tree Grid -> Int
treeCount (Node _ gs) = 1 + sum (map treeCount gs)

treeDepth :: Tree Grid -> Int
treeDepth (Node _ []) = 0
treeDepth (Node _ gs) = 1 + maximum (map treeDepth gs)

type MiniMaxTree = Tree (Grid,Player,Int)

minimax :: Tree Grid -> MiniMaxTree
minimax (Node g []) = Node (g,winner g,0) []
minimax (Node g ts) = Node (g, p',d'+1) ts'
        where
            f   = if turn g == O then minimum else maximum
            p'  = f [p | Node (_,p,_) _ <- ts']
            d'  = minimum [d | Node (_,p,d) _ <- ts', p==p']
            ts' = map minimax ts

bestmove :: MiniMaxTree -> Grid
bestmove (Node (_,best,depth) ts) = head [g' | Node (g',p',d') _ <- ts, p'==best, d'==(depth-1)]

calcbestmove :: Grid -> Grid
calcbestmove g = bestmove mmtree
    where mmtree = minimax $ gametree g (turn g)

putGrid :: Grid -> IO ()
putGrid = putStrLn . (' ':) . intersperse ' ' . map (\c -> if c=='B' then '-' else c) . unlines . map (map $ head.show)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of 
    [(x, "")] -> Just x
    _         -> Nothing

til :: Monad m => m a -> (a -> Maybe b) -> m b
til gen f = do 
    x <- gen
    case f x of
        Just y -> return y
        Nothing -> gen `til` f 

charToString :: Char -> String
charToString = replicate 1

type Input a = State String a

parseChar :: Input Char
parseChar = do
    (x:xs) <- get
    put xs
    return x

parseDigit :: Input Int
parseDigit = parseChar `til` (readMaybe . charToString)

reader :: Grid -> State String Grid
reader grid = parseDigit `til` (move piece grid)
    where piece = turn grid

slowmove :: Grid -> State () Grid
slowmove = return . calcbestmove 

traverse :: Grid -> State MiniMaxTree () 
traverse grid = do 
    (Node _ ts) <- get
    put . head . filter (\(Node (g,_,_) _) -> g==grid) $ ts

getbestmove :: State MiniMaxTree Grid
getbestmove = fmap bestmove get

quickmove :: Grid -> State MiniMaxTree Grid
quickmove grid = do
    traverse grid
    grid' <- getbestmove
    traverse grid'
    return grid'

simulate :: (Grid -> State a Grid) -> a -> (Grid -> State b Grid) -> b -> Grid -> [Grid]
simulate p1 a p2 b grid
    | done grid = return [] 
    | otherwise = (grid' : grids)
        where
            (grid', a') = runState (p1 grid) a
            grids       = simulate p2 b p1 a' grid'

fulltree = minimax $ gametree empty first
initree = Node ([],B,-1) [fulltree]

main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    inp <- getContents
    mapM_ putGrid $ simulate reader inp reader inp empty

--alpha beta pruning
--random player
