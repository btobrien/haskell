
import Prelude

hofstadter :: Int -> [Int] -> Int -> Int -> Int
hofstadter v l d n =
    if n == 0 then v
    else if (not.null) l && d == head l
        then (hofstadter v (tail l) (d+1) n)
        else (hofstadter (v+d) ((v+d):l) (d+1) (n-1))

