
import Data.Digest.Pure.SHA as SHA
import Data.ByteString.Lazy (ByteString, pack)
import Data.Word

import qualified Data.Map.Strict as Map

import qualified Data.Algabraic.Crypt.RSA as RSA

-- how many transactions must occur before a transaction is believed?
-- fixed public key
-- fixed sha id
-- address book
--
-- how to recover (&publish) chain if lost?

hash = RSA.hash 256

sign :: Show a => RSA.Key -> a -> Signed a
sign key = RSA.sign hash (RSA.defaultEncoder, key)

type Hash = Integer
type UserId = Integer

data Transaction = T { transactiontime :: Integer, receiver :: UserId, amount :: Integer }

data Block = B { blocktime :: Integer, previous :: Hash, transaction :: Signed Transaction, proof :: String }

verify :: Block -> Block -> Bool
verify prev candidate = hash prev

type Chain a = Map.Map Hash (Signed a,[Hash])

-- don't store rejects...no pow
-- first path, store as little as possible

-- append
-- balance-request
-- block-request
-- lengths

append :: Block -> Chain Block -> Chain Block
append block chain = let
    prevHash = show . head chain

balance :: UserId -> [Block] -> Integer
balance = undefined

balances :: [Block] -> [(UserId,Integer)]
balances = undefined

-- how to check if block was confirmed
