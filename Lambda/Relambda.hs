
module Relambda where

import Data.Maybe
import Lambda
import Prelude hiding ((.), (<>), id, const)
import qualified Prelude
import Plus


-- note: down is currently undefined for zero

var = Variable

print = var

infixr 1 .->
(Variable x) .-> y = Abstraction x y

infixl 7 .
(.) = Lambda.Application 

-- substitution operator
infixl 8 <>
(Abstraction var body) <> arg = substitute var arg body
x <> y = Lambda.Application x y

instance Show Lambda.Expression where
    show (Variable x) = x
    show (Abstraction var body) = var ++ ".->" ++ show body
    show (Application fn arg) = addParensIf isAbstraction fn ++ "." ++ addParensIf (\ex -> Prelude.not (isVariable ex)) arg
        where
        addParensIf needsParens expr = if needsParens expr then '(' : show expr ++ ")" else show expr

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

