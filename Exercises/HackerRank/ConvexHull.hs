
import Data.List
import Data.Array
import Text.Printf

type Point = (Double,Double)
type Line = (Double,Double); slopeOf = fst; interceptOf = snd
type Segment = (Point,Point)
type Hull = [Point]

origin = (0,0)

slope :: Segment -> Double
slope = (snd.fst)<->(snd.snd) </> (fst.fst)<->(fst.snd)

intercept :: Segment -> Double
intercept (a,b) = let m = slope (a,b) in (snd a) - m*(fst a)

toLine :: Segment -> Line
toLine = (,) . slope <*> intercept

at :: Line -> Double -> Double
at (m,b) x = m*x + b

shift :: Point -> Point -> Point
shift (x,y) (x1,y1) = ((x1-x),(y1-y))

triangle :: [Point]
triangle = [(-1,0),(0,1),(1,0)]

square :: [Point]
square = (,) <$> [0,1000] <*> [0,1000]

rhombus :: [Point]
rhombus = [(0,0),(1,1),(2,1),(1,0)]

diamond :: [Point]
diamond = [(0,1),(1,0),(0,-1),(-1,0)]

heart :: [Point]
heart = [(0,0),(0,1),(-1,2),(1,2)]

flag :: [Point]
flag = [(0,0), (0,1), (1,1), (0.5,0.5)]

flag2 :: [Point]
flag2 = [(0,0), (0,1), (0.5,0.5), (1,1)]

play :: [Point]
play = [(0,0),(0,1),(1,1)]

rplay :: [Point]
rplay = [(0,0),(0,1),(-1,1)]


slopeOrder (x,y) = (negate y/x, x)

normalize :: [Point] -> Hull
normalize points = let
    sorted = sort points
    start = head sorted
    next = shift start . last . takeWhile ((fst start ==).fst) $ sorted -- find the next one: middle points with x==0 can be discarded and won't be sorted correctly do to inf/-inf slope
    in (nub [origin,next] ++) . sortOn slopeOrder . filter ((0/=).fst) . map (shift start) $ points

convexHull :: Hull -> Hull
convexHull = reverse . foldl addPoint []
    where
    addPoint hull x | length hull < 3 = x:hull
    addPoint (c:b:a:hull) x = if isConcave a b c x then x:c:b:a:hull else addPoint (b:a:hull) x 
    isConcave a b c d = isUnder a (b,c) == isUnder d (b,c)

perimeter :: Hull -> Double
perimeter = sum . (zipWith distance <*> drop 1) . (++[origin])

isVertical :: Segment -> Bool
isVertical = uncurry $ (==) `on` fst

isUnder :: Point -> Segment -> Bool
isUnder a segment | isVertical segment = fst a < (fst.fst) segment
isUnder a segment = snd a < (toLine segment `at` fst a)

distance :: Point -> Point -> Double
distance (x,y) (x1,y1) = sqrt $ (x-x1)^2 + (y-y1)^2

solve :: [(Int, Int)] -> Double
solve = perimeter . convexHull . normalize . map (both fromIntegral)

main :: IO ()
main = do
    n <- readLn :: IO Int
    content <- getContents
    let  
        points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
        ans = solve points
    printf "%.1f\n" ans

--
on fn view x y = fn (view x) (view y)
x <+> y = (+) . x <*> y; infixl 2 <+>
x <-> y = (-) . x <*> y; infixl 2 <->
x </> y = (/) . x <*> y; infixl 1 </>
both fn (x,y) = (fn x, fn y)

