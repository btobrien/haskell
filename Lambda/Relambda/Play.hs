
import Relambda
import Relambda.Label
import Relambda.Prelude
import Prelude (($),(-), undefined)

--main = print (str "hello \n" . str "world!")
--main = print (false.str"hello ".str"world!")
--main = print (false.(self.self).str"abc")

main = run $ xnoi

ko = n.-> isZero.n ? const.put"*" .: (f.-> put"o" -. (f.(down.n).f))
xko = f.->n.-> isZero.n ? put"*" .: put"o" -. (f.f.(down.n))

noi = hnoi.hnoi.put"j".put"k".put"l"
xnoi = n.->xhnoi.put"j".put"k".put"l".n.xhnoi

hnoi = f.->x.->y.->z.->n.->
    isZero.(down.n) ? (z-.x)
    .: (f.f.y.x.z.(down.n)) -. (z-.x) -. (f.f.x.z.y.(down.n))

-- why is it important to pull the recursive abstraction point under the base check?
-- partial answer: avoids abstraction-elimination shortcut bug
xhnoi = x.->y.->z.->n.->
    isZero.(down.n) ? const.(z-.x)
    .: (f.-> (f.y.x.z.(down.n).f) -. (z-.x) -. (f.x.z.y.(down.n).f))

--lists and other datastructures
--read chars into nums
--and nums into chars
-- decompile unlambda programs
