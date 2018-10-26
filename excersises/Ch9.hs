import Data.List

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
	where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [ x | ys <- subs xs, x <- perms ys ]
--choices = concat . map perms . subs

rmone :: Eq a => [a] -> a -> [a]
rmone [] _ = []
rmone (x:xs) y = if y == x then xs else x : (rmone xs y)

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice _ [] = True
isChoice choice_set (x:xs) = if x `elem` choice_set then isChoice (rmone choice_set x) xs else False

--3
--won't terminate

splits :: [a] -> [([a],[a])]
splits [] = []
splits [_] = []
splits (x:xs) = ([x],xs) : [ (x:ls,rs) | (ls,rs) <- splits xs ]


data Expr = Val Int | App Op Expr Expr
	deriving (Eq)
data Op = Add | Sub | Mult | Div | Exp
	deriving (Eq)

instance Ord Expr where 
	e1 `compare` e2 = (length (values e1)) `compare` (length (values e2))
	-- sketch to have differnece 
	
ops :: [Op]
ops = [Add,Sub,Mult,Div,Exp]

instance Show Op where
	show Add  = "+"
	show Sub  = "-"
	show Mult = "*"
	show Div  = "/"
	show Exp  = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x >= y
valid Sub x y = True
valid Mult x y = (x /= 1) && (y /= 1) && (x >= y)
valid Div x y = (y /= 0) && (y /= 1) && (x`mod`y == 0)
valid Exp x y = (x /= 0) && (y >= 0) && (x /= 1) && (y /= 1)

apply :: Op -> Int -> Int -> Int
apply Add x y = x+y
apply Sub x y = x-y
apply Mult x y = x*y
apply Div x y = x`div`y
apply Exp x y = x^y

instance Show Expr where
	show (Val n) = show n
	show (App o l r) = brak l ++ show o ++ brak r
					   where
						 brak (Val n) = show n
						 brak e = "(" ++ show e ++ ")"



values :: Expr -> [Int]
values (Val n) = [n]
values (App _  l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

exprs :: [Int] -> [Expr]
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- splits ns, l <- exprs ls, r <- exprs rs, e <- [App o l r | o <- ops]]

type Result = (Int,Expr)

combine :: Result -> Result -> [Result]
combine (lv,le) (rv,re) = [(apply o lv rv, (App o le re)) | o <- ops, valid o lv rv]

results :: [Int] -> [Result]
results [] = []
results [n] = [(n, Val n)]
results ns = [x | (rs,ls) <- splits ns, l <- results ls, r <- results rs, x <- combine l r]

solution ns n e = isChoice ns (values e) && eval e == [n]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = map (snd) (takeWhile ((best==) . fst) sortedResults)
	where 
		sortedResults = sort [(abs (v-n),e) | nss <- choices ns, (v,e) <- results nss] -- MUCH faster than min/filter strategy ??
		best = fst (head sortedResults)

numTotalResults :: [Int] -> Int
numTotalResults = length . concat . (map results) . choices

numTotalExprs :: [Int] -> Int
numTotalExprs = length . concat . (map exprs) . choices
