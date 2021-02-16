size = 5
numQueens = 5

type Square = (Int,Int)
type Position = [Square]

--

best = maximum $ map score positions
solutions = filter ((best==).score) positions
main = print solutions

--

squares :: [Square]
squares = map (flip quotRem size) [0..(size^2 - 1)]

positions :: [Position]
positions = choose numQueens squares

score :: Position -> Int
score position = length $ filter (not . attacked position) squares

attacked :: Position -> Square -> Bool
attacked position square = let
    rank = equal fst
    file = equal snd
    diag1 = equal (uncurry (-))
    diag2 = equal (uncurry (+))
    attack = file <<||>> rank <<||>> diag1 <<||>> diag2
    in  
    any (attack square) position

--

choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose _ []     = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

--

equal f x y = (==) (f x) (f y)
p <||> p' = (||) <$> p <*> p'
p <<||>> p' = (<||>) <$> p <*> p'


