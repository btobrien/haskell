putStr' :: String -> IO ()
putStr' str = sequence_ [putChar c | c <- str]

type Board = [Int]

putBoard :: Board -> IO ()
putBoard [] = return ()
putBoard (r:rs) = do 
    putStrLn (replicate r '*')
    putBoard rs

putBoard' :: Board -> IO ()
putBoard' brd = sequence_ [putStrLn (replicate r '*') | r <- brd]

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of 
    [(x, "")] -> Just x
    _         -> Nothing

adder :: Int -> IO ()
adder x = do 
    line <- getLine
    case fmap (+x) (readMaybe line) of 
        Just x' -> adder x' 
        Nothing -> print x


getLine' :: String -> IO String
getLine' str = do
    c <- getChar
    case c of
        '\n' -> return (reverse str)
        '\DEL' -> do putStr "\b\b\b   \b\b\b"; getLine' (tail str)
        _ -> getLine' (c:str)

