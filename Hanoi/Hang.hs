
import Data.List
import Data.Char
import Control.Monad
import Control.Monad.State

main = mapM_ putStrLn . hang . filter (not.null) . lines =<< getContents

hang :: [String] -> [String]
hang [] = []
hang [x] = [x]
hang xs = let
    (left, root : right) = splitAt (length xs `div` 2) xs
    in 
    connect root (hang left) (hang right)

connect :: String -> [String] -> [String] -> [String]
connect root left right = parent : bridge children
    where
    widthOf [] = 0
    widthOf xs = length . last $ xs
    width = widthOf left + length root + widthOf right
    parent = replicate (widthOf left) ' ' ++ root ++ replicate (widthOf right) ' '
    children = zipWith (zipper width) left right

zipper :: Int -> String -> String -> String
zipper n x y = let m = n - (length x + length y) in x ++ replicate m ' ' ++ y

bridge :: [String] -> [String]
bridge [] = []
bridge (x:xs) = fillBridge x : xs
    where
    bridgeChar = '\x2508'
    fillBridge x = flip evalState x $
        popWhile isSpace <++> popWhile (not . isSpace) <++>
        fmap (map . const $ bridgeChar) (popWhile isSpace) <++> get

popWhile :: (Char -> Bool) -> State String String 
popWhile p = get >>= \str -> modify (dropWhile p) >> return (takeWhile p str)

p <++> p' = (++) <$> p <*> p'; infixl 2 <++>

