
import Relambda
import Relambda.Label
import Relambda.Prelude
import Prelude ((*),($),(-),(+), undefined)

empty = pair.(0).(0)

isempty = equal.(0) -. fst

head = fst
tail = snd
toList = flip.pair.empty
xtoList = x.->pair<>x<>empty

append = rec $ x.->l.->
    isempty.l ? base.(pair.x.empty) .| 
    pair.(l.first).(recurse.x.(l.second).recurse)


hofs = rec $ v.->l.->d.->n.->
    isZero.n ? base.v .:
    equal.d.(head.l) .|
        recurse.v.(tail.l).(d+1).n.recurse .|
        recurse.(v+d).(append.(v+d).l).(d+1).(n-1).recurse

main = run $ showNum -. (reduce hofs.(1).(pair.(1).empty).(1))

