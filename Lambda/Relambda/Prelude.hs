
module Relambda.Prelude where

import Relambda
import qualified Lambda
import Prelude (($),(-),(+),(++),putStrLn,undefined)
import qualified Prelude
import qualified Ski (compile)

id = x.->x
const = x.->y.->x
sub = x.->y.->z.->x.z.(y.z)

swallow = let f = h.->x.->h.h in f.f
self = s.->s.s

pair = x.->y.->f.->f.x.y
first = const
second = x.->y.->y
get = f.->x.->x.f
fst = x.->x.first
snd = x.->x.second
flip = f.->x.->y.->f.y.x

true = first
false = second

up = pair<>false
down = n.->isZero.n ? n .: n.second

not = x.-> x.false.true
and = x.->y.->x.y.false
or = x.->y.->x.true.y
(.&&.) = and
(.||.) = or

zero = id
isZero = n.->n.first

-- note: template relambda
num 0 = zero
num n = up <> num (n-1)

-- doesn't ski compile (?)
--recursive = let self = s.->f.(s.s) in f.->self.self

add = n.->m.->rec.n.m.rec
    where
    rec = n.->m.->
        isZero.m ? const.n .:
        (f.->f.(up.n).(down.m).f)

subtract = n.->m.->rec.n.m.rec
    where
    rec = n.->m.->
        isZero.m ? const.n .:
        (f.->f.(down.n).(down.m).f)

multiply = n.->m.->rec.n.m.rec
    where
    rec = n.->m.->
        isZero.(down.m) ? const.n .:
        (f.->add.(f.n.(down.m).f).n)
        --order of addition is perfomant

div = m.->n.->rec.m.n.rec
    where
    rec = m.->n.->
        less.m.n ? const.(0) .:
        (f.->up.(f.m.(n-m).f))
        --order of addition is perfomant

mod = m.->n.->rec.n.m.rec
    where
    rec = n.->m.-> 
        greater.n.m ? const.n .:
        (r.->r.m.(n - m).r)

less = x.->y.->rec.x.y.rec
    where
    rec = x.->y.->
        isZero.x ? const.false .:
        isZero.y ? const.true .:
        (r.->r.(down.x).(down.y).r)

greater = x.->y.->rec.x.y.rec
    where
    rec = x.->y.->
        isZero.x ? const.true .:
        isZero.y ? const.false .:
        (r.->r.(down.x).(down.y).r)

equal = x.->y.->rec.x.y.rec
    where
    rec = x.->y.->
        and.(isZero.x).(isZero.y) ? const.true
        .: isZero.x ? const.false
        .: isZero.y ? const.false
        .: (f.->f.(down.x).(down.y).f)

-- can't seem to be lam reduced
showNum = rec $ n.->
    less.(10).n ? base.(showDigit.n) .|
    showDigit.(mod.(10).n) -. recurse.(div.(10).n).recurse

showDigit = n.->
    isZero.n ? str"0" .:
    equal.(1).n ? str"1" .:
    equal.(2).n ? str"2" .: 
    equal.(3).n ? str"3" .:
    equal.(4).n ? str"4" .:
    equal.(5).n ? str"5" .:
    equal.(6).n ? str"6" .:
    equal.(7).n ? str"7" .:
    equal.(8).n ? str"8" .:
    equal.(9).n ? str"9" .: str"#"

-- builtin
instance Prelude.Num Lambda.Expression where
    fromInteger = num
    x + y = add.x.y
    x - y = subtract.x.y
    x * y = multiply.x.y
    negate = undefined
    abs = undefined
    signum = undefined

run prog =
    putStrLn (
        (\compiled -> "`r``" ++ compiled ++ getChurch ++ "i")
        (Ski.compile ((c.->prog.(c.up.zero)))))

