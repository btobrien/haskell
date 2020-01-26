

qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
	where
		smaller = [a | a <- xs, a < x]
		larger = [b | b <- xs, b > x]

main = print (qsort [2,3,4,7,5,7,4])

