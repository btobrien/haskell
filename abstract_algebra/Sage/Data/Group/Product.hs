
module Data.Group.Product where

import Data.Group
import Data.Group.Modulo

data Dihedral = Di (Int, Int); val (Di x) = x

instance Show Dihedral where
    show = show.val

instance Eq Dihedral
    where (==) = (==) `on` val
instance Ord Dihedral
    where compare = compare `on` val
instance Semigroup Dihedral 
    where (<>) = Di .: (<>) `on` val
instance Monoid Dihedral where mempty = Di.mempty
instance Group Dihedral where inv = Di.inv.val

--instance Num Dihedral where
