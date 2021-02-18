
import Data.Digest.Pure.SHA as SHA
import Data.ByteString.Lazy (ByteString, pack)
import Data.Word

import qualified Data.Algabraic.Crypt.RSA as RSA

-- how many transactions must occur before a transaction is believed?
-- fixed public key
-- fixed sha id
-- address book
--
-- how to recover (&publish) chain if lost?

sign :: Show a => RSA.Key -> a -> Signed a
sign key = RSA.sign (RSA.hash 256) (RSA.defaultEncoder, key)

type Hash = String
type UserId = Integer

data Transaction = T { timestamp :: Integer, receiver :: UserId, amount :: Integer }

data Block = B { previous :: Hash, transaction :: Signed Transaction, proof :: String }

verify :: (Block,Block) -> Bool
verify = undefined

type Chain a = Tree a

append :: Block -> Chain Block -> Chain Block
append block chain = let
    prevHash = show . head chain

trim :: Chain Block -> Chain Block
trim
balance :: UserId -> [Block] -> Integer
balance = undefined

balances :: [Block] -> [(UserId,Integer)]
balances = undefined

