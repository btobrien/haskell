
import Data.Either
import Data.List
import Control.Monad
import System.Exit
import Data.Char
import Control.Applicative
import System.IO

granularity = 2
scale :: Double -> Int
scale = floor . (granularity*)

scaleBack :: Int -> Double
scaleBack = (/granularity) . fromIntegral

data Player = Player { result :: Int, name :: String } deriving (Eq, Ord)
instance Read Player where readsPrec _ = (\x->[(x,"")]) . (Player <$> scale.read.head.tail <*> head) . words
instance Show Player where show = name <+" "+> show.scaleBack.result

readPlayers :: String -> [Player]
readPlayers = map read . filter (not.all isSpace) . lines

main = either printError (mapM_ print) . calculatePayouts . readPlayers =<< getContents
    where
    printError = (die.) $ "ERROR: results add to "++> show.scaleBack
    die x = hPutStrLn stderr x >> exitWith (ExitFailure 1)

data Payout = Payout { payout :: Int, sender :: String, receiver :: String } deriving (Eq, Ord)
instance Show Payout where show = sender <+" -> "+> receiver <+": "+> show.scaleBack.payout

calculatePayouts :: [Player] -> Either Int [Payout] 
calculatePayouts players = if net /= 0 then Left net else Right (unfoldr selectPayout players)
    where net = sum . map result $ players

selectPayout :: [Player] -> Maybe (Payout, [Player])
selectPayout players = if null players then Nothing else Just (loser `pays` winner, balance players)
    where
    winner = maximum players
    loser = minimum players
    amount = min (abs.result $ loser) (result winner) 
    pays = Payout amount `on` name
    balance =
        from winner (subtract amount) .
        from loser  (+ amount)

from :: Player -> (Int -> Int) -> [Player] -> [Player]
from player adjust = (if adjusted == 0 then id else reinsert player) . delete player
    where
    adjusted = adjust . result $ player
    reinsert = (:) . Player adjusted . name 

--

p <++> p' = (++) <$> p <*> p'; infixl 8 <++>
p +> p' = (++) <$> p <*> p'; infixl 8 +>
p <+ p' = (++) <$> p <*> const p'; infixl 8 <+
p ++> p' = (++) <$> const p <*> p'; infixl 8 ++>

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on process view x y = process (view x) (view y)
