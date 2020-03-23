
module Lambda where

import Control.Monad.State
import Data.List
import Plus

data Expression = 
    Variable String |
    Abstraction String Expression |
    Application Expression Expression deriving (Eq)

isVariable :: Expression -> Bool
isVariable (Variable _) = True; isVariable _ = False

isAbstraction :: Expression -> Bool
isAbstraction (Abstraction _ _) = True; isAbstraction _ = False

(<=>) :: Expression -> Expression -> Bool
(<=>) = (==) `on` normalize

normalize :: Expression -> Expression
normalize = alphaNormalize . reduce

reducible :: Expression -> Bool
reducible = id </=> reduce

reduce :: Expression -> Expression
reduce = etaReduce . betaReduce

(=~=) = (==) `on` alphaNormalize

alphaNormalize :: Expression -> Expression
alphaNormalize = flip evalState 0 . alpha
    where 
    alpha (Variable x) = return (Variable x)
    alpha (Application fn arg) = Application <$> alpha fn <*> alpha arg
    alpha (Abstraction var body) = modify (+1) >>
        alphaConvert <$>
            (show <$> get) <*>
            (Abstraction var <$> alpha body)

alphaConvert :: String -> Expression -> Expression
alphaConvert newVar (Abstraction var body) = Abstraction newVar (substitute var (Variable newVar) body)
alphaConvert _ x = x

betaReduce :: Expression -> Expression
betaReduce (Variable x) = Variable x
betaReduce (Abstraction var body) = Abstraction var (betaReduce body)
betaReduce (Application fn arg) = case betaReduce fn of
        Abstraction var body -> betaReduce (substitute var arg body)
        fn' -> Application fn' (betaReduce arg) 

-- Applicative Form: let arg' = betaReduce arg in arg' `seq` ...
-- note: because of Haskell's lazy evaluation, argument evaluation must be force with seq

etaReduce :: Expression -> Expression
etaReduce (Variable x) = Variable x
etaReduce (Application fn arg) = Application (etaReduce fn) (etaReduce arg)
etaReduce (Abstraction var body) = case (etaReduce body) of 
    Application fn arg -> 
        if Variable var == arg && not (var `isFreeIn` fn) then fn 
        else Abstraction var (Application fn arg)
    body' -> Abstraction var body'

substitute :: String -> Expression -> Expression -> Expression
substitute name expr (Variable x) = if x == name then expr else Variable x
substitute name expr (Application fn arg) = Application (substitute name expr fn) (substitute name expr arg)
substitute name expr (Abstraction var body) = 
    if var `isFreeIn` expr
        then substitute name expr (alphaConvert (shadow var) (Abstraction var body))
    else if var == name then Abstraction var body
    else Abstraction var (substitute name expr body)

isFreeIn :: String -> Expression -> Bool
isFreeIn var = elem var . freeVariables

freeVariables :: Expression -> [String]
freeVariables (Variable x) = [x]
freeVariables (Abstraction var body) = delete var (freeVariables body)
freeVariables (Application fn arg) = freeVariables fn `union` freeVariables arg

shadow :: String -> String
shadow = (++"'")
