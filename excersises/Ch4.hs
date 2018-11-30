
module Ch4 where

--1
halve :: [a] -> ([a],[a])
halve xs
    | even (length xs) = (take hlf_len xs, drop hlf_len xs) 
    where hlf_len = length xs `div` 2

--2
third :: [a] -> a
third xs = head (tail (tail xs))

third1 :: [a] -> a
third1 xs = xs !! 2

third2 :: [a] -> a
third2 (_:_:z:_) = z

--3
safetail2 :: [a] -> [a]
safetail2 xs = if null xs then [] else tail xs

safetail1 :: [a] -> [a]
safetail1 xs
    | null xs = []
    | otherwise = tail xs


safetail :: [a] -> [a]
safetail [] = []
safetail xs = tail xs

--4
(||) :: Bool -> Bool -> Bool
a || b 
    | a == b      = a
    | otherwise   = True

{-
False || b = b
True || _ = True

False || False = False
_ || _ = True

True || True = True
True || False = True
False || True = True
False || False = False
-}

--5/6
(&&) :: Bool -> Bool -> Bool
a && b = if a then b else False


{-
    if a == False then False else
        if b == False then False else True
-}
    

--7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x*y*z

luhnDouble :: Int -> Int
luhnDouble n
    | dub >= 10   = dub - 9
    | otherwise   = dub
    where dub = 2*n

luhn :: Int -> Int -> Int -> Int -> Bool
--luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
luhn = \a -> \b -> \c -> \d -> (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

