
module Utils where

import System.Environment
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
--import Control.Parallel
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State

(><) :: Applicative f => f a -> f b -> f (a,b)
gs >< hs = (,) <$> gs <*> hs

generate :: Eq a => (a -> a) -> a -> [a]
generate g a = (a:) . takeWhile (/=a) . tail $ iterate g a

powerset = filterM (const [True,False])
powerset' = filterM (const [False,True])

pairs xs = (,) <$> xs <*> xs
thruples xs = (,,) <$> xs <*> xs <*> xs

also :: (a -> b) -> a -> (a,b)
also = ((,)<*>)

mapAlso = map.also

ifhead :: ([a] -> b) -> [a] -> Maybe b
ifhead f xs = if null xs then Nothing else Just (f xs)

unfold :: ([a] -> (b,[a])) -> [a] -> [b]
unfold = unfoldr . ifhead

chunksOf :: Int -> [a] -> [[a]]
chunksOf = unfold . splitAt

scan :: (a -> b -> b) -> b -> [a] -> [b]
scan = scanl . flip

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = (\(front,back) -> front ++ take 1 back) . break p

on process view x y = process (view x) (view y); infixr 8 `on`
over process (viewx,viewy) x y = process (viewx x) (viewy y)

neighbors :: [a] -> [(a,a)]
neighbors = zip <$> id <*> tail

equal :: Eq a => (a,a) -> Bool
equal = uncurry (==)

equalOn :: Eq b => (a -> b) -> (a,a) -> Bool
equalOn f = uncurry ((==) `on` f)

sorted :: Ord a => [a] -> Bool
sorted = all (uncurry (<=)) . neighbors

wrap :: [a] -> [a]
wrap [] = []
wrap (x:xs) = (x:xs) ++ [x]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = let m = n `mod` length xs in drop m xs ++ take m xs

rotate' :: Int -> [[a]] -> [[a]]
rotate' 0 = id
rotate' 90 = reverse . transpose
rotate' 180 = map reverse . reverse
rotate' 270 = map reverse . transpose

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = take n xs ++ [x] ++ drop (n+1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = take n xs ++ [x] ++ drop n xs

squash :: (a -> Bool) -> a -> Maybe a
squash nothingTest x = if nothingTest x then Nothing else Just x

convergence :: Eq a => [a] -> a
convergence = fst . head . dropWhile (not.equal) . neighbors

convergenceOn :: Eq b => (a -> b) -> [a] -> a
convergenceOn f = fst . head .
    dropWhile (not . equalOn f) . neighbors

(.:) f g x y = f (g x y) 

p <&&> p' = (&&) <$> p <*> p'; infixl 1 <&&>
p <||> p' = (||) <$> p <*> p'; infixl 1 <||>
p <++> p' = (++) <$> p <*> p'; infixl 2 <++>
p <:> p' = (:) <$> p <*> p'; infixl 2 <:>
p <==> p' = (==) <$> p <*> p'; infixl 2 <==>
p +> p' = (++) <$> p <*> p'; infixl 1 +>
p <+ p' = (++) <$> p <*> const p'; infixl 1 <+
p ++> p' = (++) <$> const p <*> p'; infixl 1 ++>
--p |> p' = (++) <$> p <*> show.p'; infixl 1 |>
--p <| p' = (++) <$> p <*> const p'; infixl 1 <|
--p ||> p' = (++) <$> const p <*> show.p'; infixl 1 ||>

infixr 5 +|+
(+|+) :: [a] -> [a] -> [a]
(+|+) = (++)
--xs +|+ ys = length ys `par` (xs ++ ys)

(=~=) :: Ord a => [a] -> [a] -> Bool
(=~=) = (==) `on` Set.fromList

(<~) :: Ord a => [a] -> [a] -> Bool
(<~) = Set.isSubsetOf `on` Set.fromAscList
--(<~) = isSubsequenceOf

normalize :: Ord a => [a] -> [a] 
normalize = Set.toList . Set.fromList

choices :: [a] -> [[a]]
choices xs = concatMap (flip choose xs) [0..(length xs)]
    
choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose _ []     = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
    
pack :: Eq a => Int -> [[a]] -> [[a]]
pack = map concat .: pack'
    where
    pack' n _ | n < 0 = []
    pack' 0 _         = [[]]
    pack' _ []        = []
    pack' n (xs:xss)  = map (xs:) chosen ++ notChosen
        where
        chosen = pack' (n - length xs) (filter (null . intersect xs) xss)
        notChosen = pack' n xss

-- parition on equivalence classes
classes :: (a -> a -> Bool) -> [a] -> [[a]]
classes _ [] = []
classes relation (x:xs) = let
    (yeps,nopes) = partition (relation x) xs
    in
    (x:yeps) : classes relation nopes

-- parition on equivalence classes
classesOn :: Eq b => (a -> b) -> [a] -> [[a]]
classesOn = classes . (on (==))

getArg :: Int -> IO String
getArg index = fromMaybe "" . listToMaybe . drop index <$> getArgs

dump :: Show a => [a] -> IO ()
dump = mapM_ print

dumpstr :: [String] -> IO ()
dumpstr = mapM_ putStrLn

dumps :: Show a => [[a]] -> IO ()
dumps xss = putStr . unlines . map (intercalate " ") . (map.map) show $ xss

dumpss :: Show a => [[[a]]] -> IO ()
dumpss = mapM_ dumps

dumpx :: Show a => [[a]] -> IO ()
dumpx xss = putStr . unlines . map (intercalate " ") . (map.map) (pad wid . show)$ xss
    where wid = maximum . map (length.show) . concat $ xss

dumpt :: Show a => [[a]] -> IO ()
dumpt xss = putStr . unlines . addlines . map (intercalate " | ") . (map.map) (pad wid . show)$ xss
    where
    wid = maximum . map (length.show) . concat $ xss
    addlines ss = let n = length (head ss) in intersperse (replicate n '-') ss

___ = putStrLn 
__ = putStrLn ""
____ = putStr

pad n xs = take n $ xs ++ repeat ' ' 

normalizeOn :: Ord b => (a -> b) -> [a] -> [a]
normalizeOn f = map head . groupOn f . sortOn f

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . (on (==))

-- we have more efficient impls of these
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' = sortBy . (compare `on`)

-- we have more efficient impls of these
selectOn :: Ord b => (a -> b) -> [a] -> a
selectOn = head .: sortOn'

select :: Int -> [a] -> a
select seed xs = cycle xs !! seed

difference :: (Int,Int) -> (Int,Int) -> (Int,Int)
difference (x,y) (x',y') = (x - x', y - y')  

shift :: (Int,Int) -> (Int,Int) -> (Int,Int)
shift (x,y) (x',y') = (x + x', y + y')  

check g assumption f =
    if assumption g
    then Just (f g)
    else Nothing

binary :: Integral a => a -> [Bool]
binary = unfoldr $ \n ->
    if n==0 then Nothing
    else Just (odd n, n `div` 2)

padBinary :: [[Bool]] -> [[Bool]]
padBinary bs = let width = maximum (map length bs) in take width . (++(repeat False)) <$> bs

fromBinary :: [Bool] -> Int
fromBinary = foldr (\b -> (fromEnum b +) . (2*)) 0

zipFilter :: [Bool] -> [a] -> [a]
zipFilter bs = map snd . filter fst . zip bs

