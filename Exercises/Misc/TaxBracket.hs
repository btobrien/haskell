
import Data.Maybe
import Data.List

import Pixelate

type Income = Double
type Tax = Double
type Bracket = (Double,Double)

toMarginalRate :: [Bracket] -> [Bracket]
toMarginalRate brackets = let
    boundaries = map snd brackets
    marginals = map (abs . uncurry subtract) . (zip <*> (0:)) . map fst $ brackets
    in 
    zip marginals boundaries

singleBrackets :: [Bracket]
singleBrackets = [
    (10,0),
    (12,10275),
    (22,41775),
    (24,89075),
    (32,170050),
    (35,215950),
    (37,539900)]

jointBrackets :: [Bracket]
jointBrackets = map (\(x,y) -> if x == 37 then (x,647850) else (x,y*2)) singleBrackets

percent = (/100)
thousand = (*1000)

-- 2022 approximate minus deffered
bret :: Income
bret = thousand (260 - 40)
meredith :: Income
meredith = thousand (130 - 25)

taxes :: Income -> [Bracket] -> Tax
taxes income = sum . filter (>0) . map (tax income) . toMarginalRate
    where 
    tax income (marginalRate,start) = (percent marginalRate) * (income - start)

jointBenefit :: Income -> Income -> Tax
jointBenefit x y = taxes x singleBrackets + taxes y singleBrackets - taxes (x+y) jointBrackets 

also = ((,)<*>)

penaltyPoint :: Income -> Maybe Income
penaltyPoint income = listToMaybe . map fst . filter ((<0).snd) . map (also (jointBenefit income)) $ map thousand [0,1..1000]

dump :: Show a => [a] -> IO ()
dump = mapM_ print

explore1 =
    dump . 
    takeWhile ((>173).snd) .
    filter ((>648) . uncurry (+)) .
    filter (uncurry (>)) .
    map (fmap ((/1000).fromJust)) .
    dropWhile ((Nothing==).snd) .
    map (also (penaltyPoint.thousand)) $ [0,1..1000]

explore =
    dump . 
    takeWhile ((>173).snd) .
    filter ((>648) . uncurry (+)) .
    filter (uncurry (>)) .
    map (fmap ((/1000).fromJust)) .
    dropWhile ((Nothing==).snd) .
    map (also (penaltyPoint.thousand)) $ [0,1..2000]

pairMap :: (a -> b) -> (a,a) -> (b,b)
pairMap fn = (,).fn.fst <*> fn.snd

gridUnit :: Double -> Int -> Income
gridUnit unit = (*unit) . fromIntegral

heatMetric :: Tax -> Color
heatMetric = temperature . floor 

sigmoid :: Double -> Double
sigmoid x = x / sqrt (1 + x^2)

band1 = 100
band2 = 4000

temperature :: Int -> Color
temperature x | -band1 < x && x < band1 = White
temperature x | band1 < x && x < band2 = Cyan
temperature x | (-band1) > x && x > (-band2) = Yellow
temperature value = if value < 0 then Red else Green

example size unit = 
    putStrLn . showColors . 
    (map.map) (heatMetric . uncurry jointBenefit . pairMap (gridUnit unit)) $ grid (0,0) (floor (size/unit))

run = example 600000 4000
