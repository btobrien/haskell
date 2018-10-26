import Ch6 (halve)


data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (m `add` n)

mult :: Nat -> Nat -> Nat
mult (Succ Zero) n = n
mult (Succ m) n = (m `mult` n) `add` n

data BinTree a = Leaf a | Node (BinTree a) a (BinTree a) 

occurs :: Ord a => BinTree a -> a -> Bool
occurs (Leaf val) a = a == val
occurs (Node lt val rt) a | comparison == EQ = True
						  | comparison == LT = occurs lt a
						  | comparison == GT = occurs rt a
	where comparison = compare a val

data LeafTree a = Leaf' a | Node' (LeafTree a) (LeafTree a) 
	deriving Show

leaves :: LeafTree a -> Int
leaves (Leaf' _) = 1
leaves (Node' lt rt) = leaves rt + leaves lt

balanced :: LeafTree a -> Bool
balanced (Leaf' _) = True
balanced (Node' lt rt) = abs (leaves lt - leaves rt) <= 1
						 && balanced lt && balanced rt

balance :: [a] -> LeafTree a
balance [x] = Leaf' x
balance xs = Node' (balance left) (balance right)
	where (left,right) = halve xs

	
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x 
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Expr -> Int
eval = folde (id) (+)

size :: Expr -> Int
size = folde (\x -> 1) (+)

data Mayhaps a = Nada | Tada a

instance Eq a => Eq (Mayhaps a) where
	Nada == Nada = True
	Tada x == Tada y = x == y
	_ == _ = False

data Train a = Caboose | Car a (Train a)

instance Eq a => Eq (Train a) where
	Caboose == Caboose = True
	Car x xs == Car y ys = x == y && xs == ys 
	_ == _ = False
