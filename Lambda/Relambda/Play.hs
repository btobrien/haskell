
import Relambda
import Relambda.Label
import Relambda.Prelude
import Prelude (($),(-), undefined)

main = run $ n.->ko.n.ko

-- think Relambda is just going to make "tight" abstraction necessary
ko = n.-> isZero.n ? const.str"*" .: (r.-> str"o" -. (r.(down.n).r))
xko = n.->f.-> isZero.n ? str"*" .: str"o" -. (f.(down.n).f)
yko = f.->n.-> isZero.n ? str"*" .: str"o" -. (f.f.(down.n))

noi = n.->noi'.str"j".str"k".str"l".n.noi'
    where
    noi' = x.->y.->z.->n.->f.->
        isZero.(down.n) ? (z-.x) .: (f.y.x.z.(down.n).f) -.  (z-.x) -.  (f.x.z.y.(down.n).f)

-- why is it important to pull the recursive abstraction point under the base check?
-- partial answer: avoids abstraction-elimination shortcut bug
xnoi = n.->xnoi'.str"j".str"k".str"l".n.xnoi'
    where
    xnoi' = x.->y.->z.->n.->
        isZero.(down.n) ? const.(z-.x) .:
            (f.-> (f.y.x.z.(down.n).f) -.
            (z-.x) -.
            (f.x.z.y.(down.n).f))

--lists and other datastructures
--read chars into nums
--and nums into chars
-- decompile unlambda programs
