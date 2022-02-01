
import Data.List (group)

main = getContents >>= putStrLn .
    filter (/='1') . concatMap ((:).head<*>show.length) . group
