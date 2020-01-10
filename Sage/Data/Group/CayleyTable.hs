module Data.Group.CayleyTable where

import Utils
import Data.Group
import Data.Semigroup
import Data.Monoid hiding ((<>))

import qualified Data.Map as Map
import Data.Map (Map)

data CayleyTable a = CT { val :: a, table :: Map (a,a) a }
instance Show CayleyTable where show = show.val
instance Eq CayleyTable where (==) = (==) `on` val
instance Ord CayleyTable where compare = compare `on` val
instance Semigroup CayleyTable where
    (CT 0) <> (CT 0) = CT 0
instance Monoid CayleyTable where
	mempty = CT 0
	mappend = (<>)
instance Group CayleyTable where
    inverse (CT 0) = CT 0
    inverse (CT 1) = CT 2
    inverse (CT 2) = CT 1

nonGroup = map CT [0..2]
