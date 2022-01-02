
import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Char (ord)

import Chomp

-- add highlights?
type State = ([Bar],[Bar])

start bar = ([],[bar])
shower = show . head . snd

main = do
    hSetBuffering stdout NoBuffering
    size <- rectangle . read . fromMaybe "(7,3)" . listToMaybe <$> getArgs
    getContents >>= putStr . unlines . map shower . scanl (flip scanner) (start size) . lines

scanner :: String -> State -> State
scanner = uncurry exec . parse . words

parse :: [String] -> (String, Move)
parse (cmd:move:_) = (cmd, read move)
parse _ = ("?", undefined)

exec :: String -> Move -> (State -> State)
exec "m" = modify . flip move
exec "g" = modify . const playWinner
exec "f" = modify . playIfWinner
exec "o" = const undo
exec "O" = const (undo.undo)
exec "i" = const redo
exec "I" = const (redo.redo)
exec "?" = const id
exec c =  modify . (const.playRandom) c
--exec "x" = -- highlight red
--exec "c" = -- highlight green
--exec "c" = -- show answers


playRandom :: String -> (Bar -> Bar)
playRandom char bar = let
    mvs = moves bar
    in
    if null mvs then bar
    else move bar . (moves bar!!) . (`mod`length (moves bar)) . sum . map ord $ char

modify :: (Bar -> Bar) -> (State -> State)
modify f state@(ys,(x:xs)) = 
    if f x == x then state else ([], f x : x : xs)

--playWinnerOrRandom :: String -> (Bar -> Bar)

playIfWinner :: Move-> Bar -> Bar
playIfWinner x bar = let 
    new = move bar x
    in
    if not (winning new)
        then new
        else bar

playWinner :: Bar -> Bar
playWinner = \bar -> fromMaybe bar . fmap (move bar) . listToMaybe . winners' $ bar

undo :: State -> State
undo (ys,[x]) = (ys,[x])
undo (ys,x:xs) = (x:ys,xs)

redo :: State -> State
redo ([],xs) = ([],xs) 
redo (y:ys,xs) = (ys,y:xs) 
