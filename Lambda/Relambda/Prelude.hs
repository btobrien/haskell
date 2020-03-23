module Relambda.Prelude where

import Lambda
import Relambda
import Prelude hiding ((.), (<>), not, id, const)
import qualified Prelude 

import Data.Maybe

id = x.->x
const = x.->y.->x
sub = x.->y.->z.->x.z.(y.z)

pair = x.->y.->f.->f.x.y
first = const
second = x.->y.->y

true = first
false = second

up = pair<>false
down = n.->n.second
not = x.-> x.false.true
and = x.->y.->x.y.false
or = x.->y.->x.true.y

(?) = (.); infixr 3 ?
(.:) = (.); infixl 2 .:

isZero = n.->n.first

boolToInt = p.-> p ? 0 .: 1

add = f.->n.->m.->isZero.m ? n .: f.f.(up.n).(down.m)

greater = f.->x.->y.->isZero.x ? false .: isZero.y ? true .: f.f.(down.x).(down.y)
recursive = f.->(s.->f.(s.s)).(s.->f.(s.s))

-- auto-generate and then we can move all of below to builtin
-- ---------------------------------------------------------
label :: Lambda.Expression -> Lambda.Expression
label expr | expr =~= true = Variable "true"
label expr | expr =~= false = Variable "false"
label expr | expr =~= pair = Variable "pair"
label expr | expr =~= id = Variable "id"
label expr | isJust (isNumber expr) = Variable $ fromJust (show <$> isNumber expr)
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

run expr = label (reduce expr)

instance Num Expression where
    fromInteger 0 = id
    fromInteger num = up <> fromInteger (num-1)
    (+) = undefined
    (-) = undefined
    negate = undefined
    (*) = undefined
    abs = undefined
    signum = undefined

isNumber :: Lambda.Expression -> Maybe Int
isNumber expr | expr =~= id = Just 0
isNumber (Abstraction var (Application (Application fn arg) nest))
    | fn == (Variable var) && arg =~= false = (+1) <$> isNumber nest
isNumber _ = Nothing

