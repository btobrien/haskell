
import Relambda
import Relambda.Label
import Relambda.Prelude
import Prelude (($),(-), undefined)

--main = print (str "hello \n" . str "world!")
--main = print (false.str"hello ".str"world!")
--main = print (false.(self.self).str"abc")

main = print $ showBool.fun
xmain = print $ ko.(str"*").ko
--main = print (hanoi.(1))

fun = greater.(0).(1)

showBool = f.->f.str"T".str"F"

noi = hanoi.(2).str"j".str"k".str"l"

ko = s.->f.-> s.str"\n\n".(f.(s.s).f)

xko = n.->f.-> isZero.n ? str"*" .: str"o".(f.(down.n).f)

-- fails to compile to unlambda
    
