
module Relambda.Prelude where

import Relambda
import Prelude (($),(-))
import qualified Prelude 

id = x.->x
const = x.->y.->x
sub = x.->y.->z.->x.z.(y.z)

swallow = let f = h.->x.->h.h in f.f
self = s.->s.s

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

(?) = (.); infixr 4 ?
(.:) = (.); infixr 3 .:
(.|) = (.); infixr 1 .|

zero = id
isZero = n.->n.first

-- note: template relambda
num 0 = zero
num n = up <> num (n-1)

recursive = let self = s.->f.(s.s) in f.->self.self

add = recursive .| f.->n.->m.->
    isZero.m ? n .:
    f.(up.n).(down.m)

subtract = Prelude.undefined
multiply = Prelude.undefined

greater = recursive .| f.->x.->y.->
    isZero.x ? false .:
    isZero.y ? true .:
    f.(down.x).(down.y)

greater1 = x.->y.->rec.x.y.rec
    where
    rec = x.->y.->f.->
        isZero.x ? false
        .: isZero.y ? true
        .: f.(down.x).(down.y).f

less = x.->y.->greater.y.x

equal = x.->y.-> greater.x.y ? false .: less.x.y ? false .: true

hanoi = recursive .| f.->n.->x.->y.->z.->
    isZero.(down.n) ? x.z
    .: f.(down.n).x.z.y.(x.z).(f.(down.n).y.x.z)

