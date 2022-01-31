
import System.Environment
import Plus
import Data.Bool (bool)
import Data.Maybe (fromMaybe, listToMaybe)

main = do
    size <- read . fromMaybe "5" . listToMaybe <$> getArgs
    iteration <- read . fromMaybe "3" . listToMaybe . drop 1 <$> getArgs
    putStr . showPic $ sierpinskis size !! iteration

showPic :: [[Int]] -> String
showPic = unlines . (map.concatMap) showRun
    where
    showRun = bool <$> flip replicate ' ' . abs <*> flip replicate '^' <*> (>0)

sierpinskis :: Int -> [[[Int]]]
sierpinskis = take . (+1) <*> iterate frack . triangle . (2^)

frack :: [[Int]] -> [[Int]]
frack pic = let
    width = flip div 2 $ maximum (map maximum pic) - 1
    in
    concatMap (carve width) <$> pic
    where    
    carve width n = 
        if n <= width then [n]
        else let
        index = (n - width) - 1
        in
        index:(2*index - n):index:[]

triangle :: Int -> [[Int]]
triangle = addMargin . flip take ((:[]) <$> iterate (+2) 1)

addMargin :: [[Int]] -> [[Int]]
addMargin = map.add <$> maximum . map (sum . map abs) <*> id
    where
    add width row = let
        margin = flip div 2 $ width - sum row
        in
        filter (/=0) $ (-margin):row++[-margin]

