
module Relambda where

import Data.Maybe
import Lambda
import Prelude hiding ((.), (<>), id, const)
import qualified Prelude
import qualified Plus

put = Variable
str = Variable
var = Variable

infixr 2 .->
(Variable x) .-> y = Abstraction x y

infixl 7 .
(.) = Lambda.Application 

-- substitution operator
infixl 8 .<
(Abstraction var body) <> arg = substitute var arg body
x .< y = Lambda.Application x y

-- _arg_ must be reserved
infixr 5 -.
g -. f = var"_arg_".->g.(f.var"_arg_")
infixr 5 -..
g -.. f = var"_arg1_".->var"_arg2_".->g.(f.var"_arg1_".var"_arg2_")
infixr 5 -...
g -... f = var"_arg1_".->var"_arg2_".->var"_arg3_".->g.(f.var"_arg1_".var"_arg2_".var"_arg3_")

(?) = (.); infixr 4 ?
(.:) = (.); infixr 3 .:

base = x.->y.->x
recurse = Variable "_rec_"

infixr 3 .|
basecase .| recursive = basecase .: (recurse.->recursive)

instance Show Lambda.Expression where
    show (Variable x) = x
    show (Abstraction var body) = var ++ ".->" ++ show body
    show (Application fn arg) = addParensIf isAbstraction fn ++ "." ++ addParensIf (\ex -> Prelude.not (isVariable ex)) arg
        where
        addParensIf needsParens expr = if needsParens expr then '(' : show expr ++ ")" else show expr

rec fn =
    let vars = abstractions fn
    in
    foldr Abstraction (foldl1 Application (fn : map Variable vars ++ [fn])) vars

abstractions :: Lambda.Expression -> [String]   
abstractions (Variable _) = []  
abstractions (Application _ _) = [] 
abstractions (Abstraction var body) = var : abstractions body

a = Variable "a"
b = Variable "b"
c = Variable "c"
d = Variable "d"
e = Variable "e"
f = Variable "f"
g = Variable "g"
h = Variable "h"
i = Variable "i"
j = Variable "j"
k = Variable "k"
l = Variable "l"
m = Variable "m"
n = Variable "n"
o = Variable "o"
p = Variable "p"
q = Variable "q"
r = Variable "r"
s = Variable "s"
t = Variable "t"
u = Variable "u"
v = Variable "v"
w = Variable "w"
x = Variable "x"
y = Variable "y"
z = Variable "z"

getChurch =
    "````sii" ++
    "``s`k`s`kc" ++
    "``s``s`ks``s`k`s`ks``s`k`s`kk" ++
    "``s`k`s`kd``s`k`s`kk``s``s`ks``s``s`kskk`k" ++
    "`d``s`k`s``s`ks``s`kk" ++
    "``?0`?1`?2`?3`?4`?5`?6`?7`?8`?9" ++
    "```sii" ++
    "``s`k`s`kc" ++
    "``s``s`ks``s`kk``s`ks``s`kk``s`kd``s`kk``sii`k``ss`k`k`" ++
    "```sii" ++
    "``s`k`s`k`s`kc" ++
    "``s``s`ks``s`k`s`ks``s`k`s`kk``s`k`s`ks``s`k`s`kk" ++
    "``s`k`s`kd``s`k`s`kk``s``s`ks``s``s`kskk`k`s``s`ksk" ++
    "`k``s`k`ss``s`kkk" ++
    "`ki" ++
    "i" ++
    "`s`k" ++
    "``s``s`ksk```s``s`kski``s``s`ksk``s``s`kski" ++
    "`k``s`d`k`s`@? k" ++
    "`ki"

