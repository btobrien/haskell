

import Data.ByteString.Lazy (ByteString, pack)
import Data.Word
import Data.Maybe

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, insert, adjust)

import Utils ((.:))
import qualified Data.Algabraic.Crypt.RSA as RSA
import Data.Algabraic.Crypt.RSA (Signed, signatureOf, valueOf, signer)

import Data.Group.Modulo (Modulo, modulo, baseOf)

-- how many transactions must occur before a transaction is believed?
-- fixed public key
-- fixed sha id
-- address book
-- 0 hash is root
--
-- how to recover (&publish) chain if lost?
-- "future" timestamps should be filtered, but outside/before this pure implementation

sha = 256
hash :: Show a => a -> BlockHash
hash = RSA.hash sha

sign :: Show a => RSA.Key -> a -> RSA.Signed a
sign key = RSA.sign sha (modulo RSA.defaultEncoder (baseOf key), key)

type BlockHash = Integer
type User = Integer

data Transaction = T { timeOf :: Integer, receiverOf :: User, amountOf :: Integer }
    deriving (Eq, Ord, Show)

data Block = B { blockTimeOf :: Integer, parentOf :: BlockHash, transactionOf :: RSA.Signed Transaction, proofOf :: Int }
    deriving (Eq, Ord, Show)

type Chain a = Map BlockHash (Signed a,[BlockHash])

proven :: Show a => a -> Bool
proven = (<2^8) . hash

prove :: Block -> Block
prove = until proven incrementProof
    where
    incrementProof block = block { proofOf = proofOf block + 1 }

-- don't store rejects...no pow..could lead to dos attack
-- first pass, store as little as possible

-- append
-- balance-request

valid :: Chain Block -> Signed Block -> Bool
valid chain block = 
    validBlock block &&
    Map.notMember (hash block) chain &&
    let parentHash = (parentOf . valueOf $ block) in parentHash == 0 || case (Map.lookup parentHash chain) of
        Nothing -> False
        Just (parent,_) -> validLink (valueOf parent) (valueOf block)

validBlock :: Signed Block -> Bool
validBlock block =
    proven block &&
    RSA.verify block &&
    RSA.verify (transactionOf . valueOf $ block) &&
    blockTimeOf (valueOf block) > timeOf (valueOf . transactionOf . valueOf $ block) &&
    let
    verifer = signer block
    sender = signer (transactionOf . valueOf $ block)
    receiver = receiverOf . valueOf . transactionOf . valueOf $ block
    in
    verifer /= sender && verifer /= receiver && sender /= receiver

-- assumes parent hash matches
validLink :: Block -> Block -> Bool
validLink parent child =
    blockTimeOf parent < blockTimeOf child 
    &&
    (timeOf.valueOf) (transactionOf parent) < (timeOf.valueOf) (transactionOf child)

append :: Signed Block -> Chain Block -> Maybe (Chain Block)
append block chain = let key = hash block in
    if valid chain block
    then Just . insert key (block,[]) . adjust (addChild key) (parentOf . valueOf $ block) $ chain
    else Nothing
    where 
    addChild child (block,children) = (block, child:children)

balances :: BlockHash -> Chain Block -> Map User Integer
balances = undefined

--account :: User -> Integer -> Map User Integer -> Map User Integer
--account user amount = adjust user (+amount)

balance :: User -> BlockHash -> Chain Block -> Integer
balance user = undefined --Map.lookup .: balances 

replay :: Chain Block -> [Block]
replay = undefined

