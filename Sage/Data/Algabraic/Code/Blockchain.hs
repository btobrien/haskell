

-- how many transactions must occur before a transaction is believed?


-- cryptographic
type Hash = Key
hash :: Hash -> [Char] -> Modulo
hash = encode

type User = Key

type Transaction = ((User,User),Int)

data Signature = (User,Int)
data Signed a = S a Signature 

authentic :: Signed Int -> Bool

data Block = Block { previous :: Hash, content :: Signed Transaction, proof :: Int }

validBlock :: Block -> Bool
validBlock = undefined
suceess :: Modulo -> Bool
suceess = undefined


