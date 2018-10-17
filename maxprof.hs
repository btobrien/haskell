
maxprof :: [Int] -> Int
maxprof ns = sum [ max (y-x) 0 | (x,y) <- zip ns (tail ns)]
