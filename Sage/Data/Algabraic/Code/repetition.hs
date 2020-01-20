
import System.Environment
import Data.List
import Data.Maybe
import Utils

main = let 
    readcode "decode" = decode
    readcode "encode" = encode
    readcode  x       = error $ "(encode | decode) expected instead of \""++x++"\""
    in do
    n <- read . fromMaybe "3" . listToMaybe <$> getArgs
    code <- readcode . fromMaybe "encode" . listToMaybe . drop 1 <$> getArgs
    dump . map (code n) . lines =<< getContents

encode :: Int -> String -> String
encode = concatMap . replicate

decode :: Int -> String -> String
decode = map mostFrequent .: chunksOf
    where
    mostFrequent = head . head . sortOn' (negate . length) . group . sort
