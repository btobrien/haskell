import Data.Char
import Ch7

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

make :: Int -> [Int] -> [Int]
make n ns = take n (ns ++ repeat 0)

addpar :: [Bit] -> [Bit]
addpar bits = bits ++ [sum bits `mod` 2]

ispar :: [Bit] -> Bool
ispar = (even . sum)

checkpar :: [Bit] -> [Bit]
checkpar bits | ispar bits = init bits
			  | otherwise = error "parity mismatch"

encode :: String -> [Bit]
encode = concat . map (addpar . (make 8) . int2bin . ord)

decode :: [Int] -> String
decode = map (chr . bin2int . checkpar) . (chop 9)

transmit :: String -> String
transmit = decode . tail . encode

-- interesting note: parity check will catch all errors where atleast one char is "odd"
