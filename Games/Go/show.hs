
import System.IO
import Data.List
import Data.Char

import Plus (chunksOf)
import Go

main = do
    hSetBuffering stdout NoBuffering
    mapM_ (putStr . showBoard . read) . lines =<< getContents

showBoard :: Board -> String
showBoard xs = let z = size xs
    in
    (++"\n") .
    intercalate "\n" .
    map (intersperse connect) .
    chunksOf z $
    map (showSquare z) (squares xs)

showSquare :: Size -> Annotated Square -> Char
showSquare z square =
    if occupied (value square)
        then showPiece (piece <$> square)
        else showCoordinate z (location <$> square)

showPiece :: Annotated Piece -> Char
showPiece ("", Black) = black 0
showPiece ("", White) = white 0
showPiece ((c:_), Black) | isLower c = black 2
showPiece ((c:_), White) | isLower c = white 2
showPiece ((c:_), Black) = black 1
showPiece ((c:_), White) = white 1

showCoordinate :: Size -> Annotated Coordinate -> Char
showCoordinate z x = head $ annotation x ++ [gridAt z (value x)]

gridAt :: Size -> Int -> Char
gridAt z x | isTop z x && isLeft z x = '\x250c'
gridAt z x | isTop z x && isRight z x = '\x2510'
gridAt z x | isBottom z x && isLeft z x = '\x2514'
gridAt z x | isBottom z x && isRight z x = '\x2518'
gridAt z x | isTop z x = '\x252c'
gridAt z x | isBottom z x = '\x2534'
gridAt z x | isRight z x = '\x2524'
gridAt z x | isLeft z x = '\x251c'
gridAt z x | otherwise = '\x253c'

isLeft :: Size -> Int -> Bool
isLeft z = (0==) . (`mod`z)

isTop :: Size -> Int -> Bool
isTop z = (<z)

isBottom :: Size -> Int -> Bool
isBottom z = ((z-1)==) . (`div`z)

isRight :: Size -> Int -> Bool
isRight z = ((z-1)==) . (`mod`z)

black :: Int -> Char
black 0 = '\x25cf'
black 1 = '\x2688'
black 2 = '\x2689'

white :: Int -> Char
white 0 = '\x25cb'
white 1 = '\x2686'
white 2 = '\x2687'

connect :: Char
connect = '\x2500'
