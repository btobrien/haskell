
import Utils (getArg)

import Data.Algabraic.Code (Code, Mode, encode, decode)
import Data.Algabraic.Code.Repetition (repetition)
import Data.Algabraic.Code.Hamming (hamming)

main = do
    code <- readcode <$> getArg 0
    mode <- readmode <$> getArg 1
    mapM_ putStrLn . map (mode code) . lines =<< getContents

readcode :: String -> Code
readcode "repetition" = repetition 3
readcode "hamming" = hamming
readcode [] = repetition 3
readcode x = error $
    "(repetition | hamming) expected -- \""
    ++ x ++ "\" not supported"

readmode :: String -> Code -> Mode
readmode "decode" = decode
readmode "encode" = encode
readmode [] = encode
readmode x = error $
    "(encode | decode) expected -- \""
    ++ x ++ "\" not supported"
