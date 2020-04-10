
module Ski where

import Text.ParserCombinators.Parsec hiding (Parser)
import Control.Monad
import Data.Either
import Lambda hiding (Expression(..))
import qualified Lambda (Expression(..))
import Data.List (intercalate, replicate, length)

data Expression = Application Expression Expression | S | K | I | Variable String deriving (Eq)

relambda :: Ski.Expression -> Lambda.Expression
relambda (Variable x) = Lambda.Variable x
relambda (Application fn arg) = Lambda.Application (relambda fn) (relambda arg)
relambda I = Lambda.Abstraction "x" (Lambda.Variable "x")
relambda K = Lambda.Abstraction "k" (Lambda.Abstraction "_" (Lambda.Variable "k"))
relambda S =
    Lambda.Abstraction "a" (Lambda.Abstraction "b" (Lambda.Abstraction "c" (
        Lambda.Application
            (Lambda.Application
                (Lambda.Variable "a")
                (Lambda.Variable "c"))
            (Lambda.Application
                (Lambda.Variable "b")
                (Lambda.Variable "c")))))

unlambda :: Lambda.Expression -> Ski.Expression
unlambda x | x =~= relambda S = S
unlambda x | x =~= relambda K = K
unlambda (Lambda.Variable x) = Variable x
unlambda (Lambda.Application fn arg) = Application (unlambda fn) (unlambda arg)
unlambda (Lambda.Abstraction var body) = eliminate var (unlambda body)

contains :: String -> Ski.Expression -> Bool
contains name (Variable x) = name == x
contains name (Application fn arg) = contains name fn || contains name arg
contains name _ = False

eliminate :: String -> Ski.Expression -> Ski.Expression
eliminate var expr@(Application fn arg) = 
    if not (contains var expr) then Application K expr
    else Application (Application S (eliminate var fn)) (eliminate var arg)
    --Application (Application S (eliminate var fn)) (eliminate var arg)
eliminate var x = if Variable var == x then I else Application K x

instance Show Expression where
    show S = "s"
    show K = "k"
    show I = "i"
    show (Variable x) = '[' : x ++ "]"
    show (Application fn arg) = show fn ++ addParens arg
        where
        addParens (Application fn arg) = '(' : show (Application fn arg) ++ ")"
        addParens expr = show expr

prefix :: Expression -> String
prefix S = "s"
prefix K = "k"
prefix I = "i"
--prefix (Variable x) =  (++"i") . ("`d`."++) . intercalate "`." . map (:[]) . reverse $ x
prefix (Variable x) =  (++"i") . ("`."++) . intercalate "`." . map (:[]) . reverse $ x
prefix (Application x y) = '`' : prefix x ++ "`d" ++ prefix y

compile = prefix . unlambda

ski :: String -> Expression
ski = fromRight undefined . parse expression []

expression = application <|> builtin <|> variable

application = char '`' >> (Application <$> expression <*> expression)

builtin =
    (char 's' >> return S) <|>
    (char 'k' >> return K) <|>
    (char 'i' >> return I)

variable = Variable . (:[]) <$> (char '.' >> anyChar)



