
import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Char as Char
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
exec "n" = modify . flip move
exec "w" = modify . const playWinner
exec "g" = modify . playIfWinner
exec "o" = const undo
exec "O" = const (undo.undo)
exec "i" = const redo
exec "I" = const (redo.redo)
exec "?" = const id
exec str | str == "N" || Char.isNumber (head str) = modify . \move -> playWinnerOrRandom (str ++ show move) -- could also seed from redo stack
exec str = modify . \move -> playRandom (str ++ show move)

--exec "x" = -- highlight red
--exec "c" = -- highlight green
--exec "a" = -- show answers


-- highlights
-- play random but prefer "smaller" moves

playRandom :: String -> (Bar -> Bar)
playRandom char bar = let
    mvs = moves bar
    in
    if null mvs then bar
    else move bar . (moves bar!!) . (`mod`length (moves bar)) . sum . map ord $ char

modify :: (Bar -> Bar) -> (State -> State)
modify f state@(_,(x:xs)) = 
    if f x == x then state else ([], f x : x : xs)

playWinnerOrRandom :: String -> (Bar -> Bar)
playWinnerOrRandom str bar = let
    new = playWinner bar
    in
    if new == bar then playRandom str bar else new

playIfWinner :: Move -> Bar -> Bar
playIfWinner x bar = let 
    new = move bar x
    in
    if not (winning new)
        then new
        else bar

playWinner :: Bar -> Bar
playWinner = \bar -> fromMaybe bar . fmap (move bar) . listToMaybe . winningMoves $ bar

-- zipper pattern
undo :: State -> State
undo (ys,[x]) = (ys,[x])
undo (ys,x:xs) = (x:ys,xs)

redo :: State -> State
redo ([],xs) = ([],xs) 
redo (y:ys,xs) = (ys,y:xs) 
