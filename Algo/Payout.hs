
import Data.Either
import Data.List

type Result = (Int, String); valueOf = fst; nameOf = snd; 
type Payout = (Int, (String, String))

main = getContents >>= putStrLn . either showError showPayouts . calculatePayouts . readResults
    where
    readResults = map ((\(name:val:_) -> (read val, name)) . words) . lines
    showPayouts = unlines . map (\(val, (from, to)) -> from ++ " -> " ++ to ++ ": " ++ show val)
    showError = ("ERROR: results add to "++) . show 
    
calculatePayouts :: [Result] -> Either Int [Payout] 
calculatePayouts results = if net /= 0 then (Left net) else Right (unfoldr selectPayout (losers,winners))
    where
    net = sum . map valueOf $ results
    (losers,winners) = partition ((<0) . valueOf) results

selectPayout :: ([Result], [Result]) -> Maybe (Payout, ([Result],[Result]))
selectPayout (losers,winners) = if null winners || null losers then Nothing else Just (payout, adjusted)
    where
    winner = maximum winners
    loser = minimum losers
    amount = min (abs . valueOf $ loser) (valueOf winner) 
    payout = (amount, (nameOf loser, nameOf winner))
    adjusted = (,) 
        (adjust (nameOf loser)  (valueOf loser  + amount) losers)
        (adjust (nameOf winner) (valueOf winner - amount) winners)

adjust :: String -> Int -> [Result] -> [Result]
adjust name new = filter ((0/=) . valueOf) . ((new,name):) . filter ((name/=) . nameOf)
