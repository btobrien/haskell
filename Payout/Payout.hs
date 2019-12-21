
import Data.Either
import Data.List
import Control.Monad
import System.Exit
import Data.Char
import System.IO

import Player

die x = hPutStrLn stderr x >> exitWith (ExitFailure 1)

main = either printError printAll . calculatePayouts . readPlayers =<< getContents
    where printError = (die.) $ "ERROR: results add to "++> show.scaleBack

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
