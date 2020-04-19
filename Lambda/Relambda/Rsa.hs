
import Relambda
import Relambda.Label
import Relambda.Prelude
import Prelude ((*),($),(-),(+), undefined)

--main = run $ showNum -. code.(encoder.(4).(3).(5))
main = run $ showNum -. (n.->selectUnit.n.(5).(1))
--main = run $ showNum -. powerMod.(5).(2)

inverse = id

isUnit = const.(const.true)

-- seems to be a bug here
selectUnit = rec $ s.->m.->c.-> 
    let found = isUnit.m.c
    in
    and.(isZero.s).found ? c .| (0)
    --recurse.(found.(down.s).s).m.(up.c).recurse

powerMod = rec $ n.->x.->e.->
    isZero.(down.e) ? base.(mod.n.x) .|
    mod.n.((recurse.n.x.(down.e).recurse) * x)

modulo = pair; baseOf = fst; value = snd

encoder = fst -... generate
decoder = snd -... generate
generate = s.->p.->q.->
    let
    n = p * q
    m = (p-1) * (q-1)
    e = selectUnit.s.m.(1)
    d = inverse.e
    in pair.(modulo.n.e).(modulo.n.d)

code = e.->x.-> powerMod.(baseOf.e).x.(value.e)
