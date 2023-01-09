

dump :: Show a => [a] -> IO ()
dump = mapM_ print

f .: g = \x y -> f (g x y)

price = 645000.0
loan = 483750.0
years = 30 :: Int
rate = 4.625
capitalGainsTax = 0.15

monthsOf = (12*)
monthly = (/1200)

interestPercentage years rate = (fromIntegral (monthsOf years) * installment (monthsOf years) (monthly rate)) - 1 
payment years rate = installment (monthsOf years) (monthly rate) * loan

equityPercentage loanYears rate duration = flip subtract 1 $ amortize (monthsOf loanYears) (monthly rate) !! (monthsOf duration)

--
--

effectiveRate :: Int -> Double -> Double
effectiveRate n i = (1 + i/(fromIntegral n))^n - 1

installment :: Int -> Double -> Double
installment n i = i * (total n i / interest n i)
    where
    total n i = (1 + i) ^ n
    interest = subtract 1 .: total

amortize :: Int -> Double -> [Double]
amortize n i = takeWhile (>=0) $ iterate (pay (installment n i) i) 1
    where
    pay amount i balance = (1+i) * balance - amount

schedule :: Int -> Double -> [[Double]]
schedule n i = zipWith (\x y -> [x,y-x,y]) (map (abs . subtract 1) (amortize n i )) $ iterate (+(installment n i)) 0

scale :: Double -> [[Double]] -> [[Int]]
scale loan = (map.map) (floor.(*loan))

condense :: Int -> [a] -> [(Int,a)]
condense m = map (\(x,y) -> (div x m, y)) . filter ((0==).(`mod`m).fst) . zip [0..]

deltas :: [[Double]] -> [[Double]]
deltas = (([]:).) $ zipWith (zipWith subtract) <*> tail

compound :: Double -> [Double]
compound i = iterate (*(1+i)) 1

-- future bond value of unit dollar cash flow
futureBondValue n = sum . take n . compound

gainsAdjustedBondValue :: Double -> Int -> Double -> Double
gainsAdjustedBondValue gainsTax n rate =
    let
    basis = fromIntegral n
    value = futureBondValue n rate
    in (1 - gainsTax) * (value - basis) + basis 


-- include ltv and appreciation in equity
-- subtract property taxes / hoi&hoa
-- account for inflation (assume the property appreciation == inflation?)

investingAdvantage long short interest interestBenefit marketReturns = 
    let
    extra = installment (monthsOf short) (monthly (interest - interestBenefit)) - installment (monthsOf long) (monthly interest)
    loanLeft = amortize (monthsOf long) (monthly interest) !! monthsOf short
    investments = extra * (gainsAdjustedBondValue capitalGainsTax (monthsOf short) (monthly marketReturns))
    in
    --[extra, loanLeft, investments]
    investments - loanLeft




