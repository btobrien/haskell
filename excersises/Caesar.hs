import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
	| isLower c = int2let((let2int c + n) `mod` 26)
	| otherwise = int2let(((let2int c + n - diff) `mod` 26) + diff)
		where diff = (ord 'A' - ord 'a')
