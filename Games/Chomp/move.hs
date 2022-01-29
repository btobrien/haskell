
import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Char as Char
import Data.Char (ord)
import Plus
import Data.List (sortOn)

import Chomp

type State = ([Bar],[Bar])

start bar = ([],[bar])
shower = show . head . snd

main = do
    hSetBuffering stdout NoBuffering
    size <- rectangle . read . fromMaybe "(12,3)" . listToMaybe <$> getArgs
    getContents >>= putStr . unlines . map shower . scanl (flip scanner) (start size) . lines

scanner :: String -> State -> State
scanner = uncurry exec . parse . words

parse :: [String] -> (String, Move)
parse (cmd:move:_) = (cmd, read move)
parse _ = ("?", undefined)

exec :: String -> Move -> (State -> State)
exec "m" = modify . flip move
exec ";" = exec "w"
exec "w" = modify . const playWinner
exec "M" = modify . playIfWinner
exec "o" = const undo
exec "O" = const (undo.undo)
exec "i" = const redo
exec "I" = const (redo.redo)
exec "?" = const id
exec str | all Char.isNumber str = modify . const (playWinnerOrRandom (read str))
exec str = modify . const (playRandom (sum (map ord str)))

playRandom :: Int -> (Bar -> Bar)
playRandom n bar = let
    mvs = moves bar
    in
    if null mvs then bar
    else -- randomize, but prefer larger bars..
    let bars = reverse . sortOn sum $ (map.move<*>moves) bar
    in
    bars !! squish (length bars) n

squish :: Int -> Int -> Int
squish max = (`div`(max^4)) . (^5) . (`mod`max)

modify :: (Bar -> Bar) -> (State -> State)
modify f state@(_,(x:xs)) = 
    if f x == x then state else ([], f x : x : xs)

playWinnerOrRandom :: Int -> (Bar -> Bar)
playWinnerOrRandom n bar = let
    new = playWinner bar
    in
    if new == bar then playRandom n bar else new

playIfWinner :: Move -> Bar -> Bar
playIfWinner _ [] = []
playIfWinner x bar = let 
    new = move bar x
    in
    if not (winning new)
        then new
        else bar

playWinner :: Bar -> Bar
playWinner [] = []
playWinner bar = 
    fromMaybe bar . fmap (move bar) .
    listToMaybe . winningMoves $ bar

-- zipper
undo :: State -> State
undo (ys,[x]) = (ys,[x])
undo (ys,x:xs) = (x:ys,xs)

redo :: State -> State
redo ([],xs) = ([],xs) 
redo (y:ys,xs) = (ys,y:xs) 
