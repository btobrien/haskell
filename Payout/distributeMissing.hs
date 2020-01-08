
import Control.Applicative
import Data.List
import Data.Char
import Player

main = printAll . distribute . readPlayers =<< getContents 

data Adjustment = Adjustment { adjusted :: Int, player :: Player } deriving (Eq, Show)
value = (+) <$> adjusted <*> result.player
toAdjustment = Adjustment 0
fromAdjustment =  Player <$> value <*> name . player 

instance Ord Adjustment where
    compare  = compare `on` contempt where
        contempt = (,) <$> ratio <*> abs.value where
            ratio = (/)
                <$> fromIntegral . value
                <*> fromIntegral . (^2) . adjusted

distribute :: [Player] -> [Player]
distribute players = map fromAdjustment . until distributed adjust $ adjustments
        where 
        adjustments = map toAdjustment players
        missing = sum . map value
        distributed = (==0) . missing
        adjust = if (missing adjustments) > 0
            then from maximum (subtract 1)
            else from minimum (+1)

from :: ([Adjustment] -> Adjustment) -> (Int -> Int) -> [Adjustment] -> [Adjustment]
from select increment adjusters = reinsert adjustee . delete adjustee $ adjusters
    where
    adjustee = select adjusters
    reinsert = (:) . (Adjustment <$> increment . adjusted <*> player)

