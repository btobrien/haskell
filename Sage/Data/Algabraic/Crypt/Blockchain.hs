

import Data.ByteString.Lazy (ByteString, pack)
import Data.Word
import Data.Maybe

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, insert, adjust)

import Utils ((.:))
import qualified Data.Algabraic.Crypt.RSA as RSA
import Data.Algabraic.Crypt.RSA (Signed, signatureOf, valueOf)

import Data.Group.Modulo (Modulo, modulo, baseOf)

-- how many transactions must occur before a transaction is believed?
-- fixed public key
-- fixed sha id
-- address book
--
-- how to recover (&publish) chain if lost?


sha = 256

sign :: Show a => RSA.Key -> a -> RSA.Signed a
sign key = RSA.sign sha (modulo RSA.defaultEncoder (baseOf key), key)

proven :: Show a => a -> Bool
proven = (<2^8) . RSA.hash sha

type BlockHash = Integer
type User = Integer

data Transaction = T { transactionTimeOf :: Integer, receiverOf :: User, amountOf :: Integer }
    deriving (Eq, Ord, Show)

data Block = B { blockTimeOf :: Integer, parentOf :: BlockHash, transactionOf :: RSA.Signed Transaction, proofOf :: String }
    deriving (Eq, Ord, Show)

type Chain a = Map BlockHash (Signed a,[BlockHash])

hash :: Signed Block -> BlockHash
hash = RSA.hash sha . valueOf

-- don't store rejects...no pow
-- first path, store as little as possible

-- append
-- balance-request
-- block-request (encapsulates whether it's been cleared?)


valid :: Chain Block -> Signed Block -> Bool
valid chain block =
    RSA.verify block 
    &&
    Map.notMember key chain
    &&
    (blockTimeOf . valueOf $ block) > transactionTimeOf (valueOf . transactionOf . valueOf $ block)
    -- &&
    --transactionTimeOf (transactionOf block)
    where 
    key = hash block

addChild :: BlockHash -> (Signed Block, [BlockHash]) -> (Signed Block, [BlockHash])
addChild child (block,children) = (block, child:children)

append :: Signed Block -> Chain Block -> Maybe (Chain Block)
append block chain =
    if valid chain block
    then Just . insert key (block,[]) . adjust (addChild key) parent $ chain
    else Nothing
    where
    parent = fromJust . Map.lookup . parentOf . valueOf $ block
    key = hash block

-- 0 hash is root

balances :: BlockHash -> Chain Block -> Map User Integer
balances = undefined

--account :: User -> Integer -> Map User Integer -> Map User Integer
--account user amount = adjust user (+amount)

balance :: User -> BlockHash -> Chain Block -> Integer
balance user = undefined --Map.lookup .: balances 

-- how to check if block was confirmed
--
-- hash collision???
