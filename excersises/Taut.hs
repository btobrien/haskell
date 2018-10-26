
import Assoc

data Prop = Const Bool
		  | Var Char
		  | Not Prop
		  | And Prop Prop
		  | Or Prop Prop
		  | Imply Prop Prop
		  | Equiv Prop Prop

type Sub = Assoc Char Bool

eval :: Prop -> Sub -> Bool
eval (Const b) _ = b
eval (Var x) s = head (find s x)
eval (Not p) s = not (eval p s)
eval (And p q) s = eval p s && eval q s
eval (Or p q) s = eval p s || eval q s
eval (Imply p q) s = eval p s <= eval q s
eval (Equiv p q) s = eval p s == eval q s

vars :: Prop -> [Char]
vars (Const b) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = [ b:bs | b <- [True,False], bs <- bools (n-1) ]

subs :: Prop -> [Sub]
subs p = map (zip vs) (bools (length vs))
	where vs = (rmdups . vars) p

isTaut :: Prop -> Bool
isTaut p = and (map (eval p) (subs p))
