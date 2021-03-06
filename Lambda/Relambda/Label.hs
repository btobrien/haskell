
module Relambda.Label where

import Lambda
import Relambda
import Relambda.Prelude
import Prelude hiding ((.), (<>), not, id, const, subtract)
import qualified Prelude 
import Data.List (last)

import Data.Maybe

-- would be nice to auto-generate
label :: Lambda.Expression -> Lambda.Expression
label expr | expr =~= true = Variable "true"
label expr | expr =~= false = Variable "false"
label expr | expr =~= pair = Variable "pair"
label expr | expr =~= id = Variable "id"
label expr | isJust (isNumber expr) = Variable (fromJust (show <$> isNumber expr))
label expr | expr =~= const = Variable "const"
label expr | expr =~= sub = Variable "sub"
label expr | expr =~= isZero = Variable "isZero"
label expr | expr =~= up = Variable "up"
label expr | expr =~= down = Variable "down"
label expr | expr =~= add = Variable "add"
label (Variable x) = (Variable x)
label (Abstraction var body) = Abstraction var (label body)
label (Application fn arg) = Application (label fn) (label arg)
-- implement traversable -- we've seen this patter a lot

evaluate expr = label (Lambda.reduce expr)
reduce = Lambda.reduce

isNumber :: Lambda.Expression -> Maybe Int
isNumber expr | expr =~= id = Just 0
isNumber (Abstraction var (Application (Application fn arg) nest))
    | fn == (Variable var) && arg =~= false = (+1) <$> isNumber nest
isNumber _ = Nothing
