
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

data LeafTree a = Leaf a | Node (BinTree a) (BinTree a) 
