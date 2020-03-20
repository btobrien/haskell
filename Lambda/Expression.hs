
module Expression where

import Data.List
import Plus

-- deBrujn indices - alphs equivalence
-- implement eta reductions 
-- introduce definitions and labeling
-- preprocess/import definitions
-- static type checking ?
-- undefined variables interpreted as string literals?
-- laziness / dely + side effecst

data Expression = 
    Variable String |
    Abstraction String Expression |
    Application Expression Expression
    deriving (Show, Eq)

freeVariables :: Expression -> [String]
freeVariables (Variable x) = [x]
freeVariables (Abstraction var body) = delete var (freeVariables body)
freeVariables (Application fn arg) = freeVariables fn `union` freeVariables arg

substitute :: String -> Expression -> Expression -> Expression
substitute name expr (Variable x) = if x == name then expr else Variable x
substitute name expr (Application fn arg) = Application (substitute name expr fn) (substitute name expr arg)
substitute name expr (Abstraction var body) = 
    if var `elem` freeVariables expr
        then substitute name expr (alphaConvert (shadow var) (Abstraction var body))
    else if var == name then Abstraction var body
    else Abstraction var (substitute name expr body)

alphaConvert :: String -> Expression -> Expression
alphaConvert newVar (Abstraction var body) = Abstraction newVar (substitute var (Variable newVar) body)
alphaConvert _ x = x

shadow :: String -> String
shadow = (++"'")

betaReduce :: Expression -> Expression
betaReduce (Variable x) = Variable x
betaReduce (Abstraction var body) = Abstraction var (betaReduce body)
betaReduce (Application fn arg) = case (betaReduce fn) of
        Abstraction var body -> betaReduce (substitute var arg body)
        fn' -> Application fn' (betaReduce arg) 

-- Applicative Form: Abstraction var body -> let arg' = betaReduce arg in arg' `seq` betaReduceA (substitute var arg' body)

etaReduce :: Expression -> Expression
etaReduce (Variable x) = Variable x
etaReduce (Application fn arg) = Application (etaReduce fn) (etaReduce arg)
etaReduce (Abstraction var body) = case (etaReduce body) of 
    Application fn arg -> 
        if Variable var == arg && not (var `elem` freeVariables fn) then fn 
        else Abstraction var (Application fn arg)
    body' -> Abstraction var body'

reduce = etaReduce . betaReduce

alphaNormalize :: Expression -> Expression
alphaNormalize = id

normalize :: Expression -> Expression
normalize = alphaNormalize . reduce

(<=>) :: Expression -> Expression -> Bool
(<=>) = (==) `on` normalize

