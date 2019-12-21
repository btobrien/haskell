
double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` (length xs)
	where
		a = 10
		xs = [1,2,3,4,5]

last1 xs = head (reverse xs)

last2 xs = xs !! ((length xs) - 1)

init1 xs = reverse (tail (reverse xs))

init2 xs = take ((length xs) - 1) xs


