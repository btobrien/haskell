

dump :: Show a => [a] -> IO ()
dump = mapM_ print

f .: g = \x y -> f (g x y)

rate = 0.04625/12
price = 645000.0
loan = 483750.0
ltv = loan/price
terms = 30*months :: Int
months = 12
taxes = 10000 :: Double -- ?
inflation = 0.03/12 -- simple -- ?
appreciation = inflation -- ?

homeOwners = 300

payment = installment terms rate * loan

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

inflate :: Double -> [[Double]] -> [[Double]]
inflate i = zipWith (map . (flip(/))) (compound i)

property :: Double -> Double -> [[Double]]
property appreciation tax = map (\x -> [x, tax*x]) (compound appreciation)

-- include ltv and appreciation in equity
-- subtract property taxes / hoi&hoa
-- account for inflation (assume the property appreciation == inflation?)
