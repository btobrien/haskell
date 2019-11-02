import Data.Either
import Data.List

type Player = (Int, String); valueOf = fst; nameOf = snd; 
type Payout = (Int, (String, String))

main = getContents >>= putStrLn . either showError showPayouts . calculatePayouts . readPlayer
    where
    readPlayer = map ((\(name:val:_) -> (read val, name)) . words) . lines
    showPayouts = unlines . map (\(val, (from, to)) -> from ++ " -> " ++ to ++ ": " ++ show val)
    showError = ("ERROR: results add to "++) . show 
    
calculatePayouts :: [Player] -> Either Int [Payout] 
calculatePayouts players = if net /= 0 then Left net else Right (unfoldr selectPayout players)
    where net = sum . map valueOf $ players

selectPayout :: [Player] -> Maybe (Payout, [Player])
selectPayout players = if null players then Nothing else Just (payout, updated)
    where
    loser = minimum players
    winner = maximum players
    amount = min (abs . valueOf $ loser) (valueOf winner) 
    payout = (,) amount (nameOf loser, nameOf winner)
    updated =
        update (nameOf loser)  (valueOf loser  + amount) .
        update (nameOf winner) (valueOf winner - amount) $ players

update :: String -> Int -> [Player] -> [Player]
update name new = filter ((0/=) . valueOf) . ((new,name):) . filter ((name/=) . nameOf)
