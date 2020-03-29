
import Relambda
import Relambda.Label
import Relambda.Prelude
import Prelude (($),(-), undefined)

--main = print (str "hello \n" . str "world!")
--main = print (false.str"hello ".str"world!")
--main = print (false.(self.self).str"abc")

xmain = print $ noi
main = print $ ko.4.ko
--main = print (hanoi.(1))

fun = greater.(0).(0)

showBool = f.->f.str"T".str"F"

noi = hanoi.(7).str"j".str"k".str"l"

ko = n.->f.-> isZero.n ? str"*" .: str"o".(f.(down.n).f)
-- fails to compile to unlambda
    
