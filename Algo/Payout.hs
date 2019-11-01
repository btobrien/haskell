
import Data.Either
import Data.List
import Control.Applicative

type Name = String
type Result = (Int, Name); resultOf = fst; nameOf = snd; 
type Payout = (Int, (Name, Name)) --; payoutOf = fst; namesOf = snd
type Missing = Int

readResult :: String -> Result
readResult = (\(name:val:_) -> (read val, name)) . words

showPayout :: Payout -> String
showPayout (val, (from, to)) = from ++ " -> " ++ to ++ ": " ++ (show val)

showMissing :: Missing -> String
showMissing val = "ERROR: results add to " ++ show val 

main = do 
    inp <- getContents
    let results = map readResult . lines $ inp
    putStrLn . either showMissing (unlines . map showPayout) . balance $ results
    
balance :: [Result] -> Either Missing [Payout] 
balance results = if net /= 0 then (Left net) else Right (unfoldr payout splitResults)
    where
    net = sum (map resultOf results)
    splitResults = partition ((<0) . resultOf) results

payout :: ([Result], [Result]) -> Maybe (Payout, ([Result],[Result]))
payout (losers,winners) = if null winners || null losers then Nothing else Just (payout,adjusted)
    where
    winner = maximum winners
    loser = minimum losers
    amount = min (abs (resultOf loser)) (resultOf winner) 
    payout = (amount, (nameOf loser, nameOf winner))
    adjusted = (,) 
        (adjust (nameOf loser) (resultOf loser + amount) losers)
        (adjust (nameOf winner) (resultOf winner - amount) winners)

adjust :: Name -> Int -> [Result] -> [Result]
adjust name new = filter ((0/=) . resultOf) . ((new,name):) . filter ((name/=) . nameOf)
