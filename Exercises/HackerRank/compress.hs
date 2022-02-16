
import Data.Bool (bool)
import Data.List (group)

main = prefix

run = getContents >>= putStrLn .
    concatMap ((:).head<*>(bool [] <*> (/="1")).show.length) . group

prefix = getContents >>= putStr . unlines .
    ((:).(map fst).fst <*> ((:).(map fst)<*>(:[]).(map snd)).snd) .
    span (uncurry (==)) . (zip.head<*>last) . lines
    

showTri :: ([(Char,Char)],[(Char,Char)]) -> [String]
showTri = ((:).(map fst).fst <*> ((:).(map fst)<*>(:[]).(map snd)).snd)

    

