
module Data.Group.Dihedral where

import Utils
import Data.Group
import Data.Group.Modulo

instance Semigroup Bool
    where 
    True <> True = False
    False <> False = False
    _ <> _ = True
instance Monoid Bool where mempty = False 
instance Group Bool where inv = id 

newtype Dihedral = Di (Bool,Modulo); val (Di x) = x

reflected :: Dihedral -> Bool
reflected = fst.val

xcompose :: (Bool,Modulo) -> (Bool,Modulo) -> (Bool,Modulo)
xcompose (a,b) (c,d) = if c
    then (a<>c, inv b <> d) 
    else (a, b<>d)

instance Show Dihedral where
    show = show.val

instance Eq Dihedral where
    (==) = (==) `on` val

instance Ord Dihedral where
    compare = compare `on` val

instance Semigroup Dihedral where
    (<>) = Di .: xcompose `on` val

instance Monoid Dihedral where
    mempty = Di mempty

instance Group Dihedral where
    inv d = if reflected d
        then d
        else reflect <> d <> reflect

rotateOn n = Di (False, 1 <+> base n)
reflect = Di (True, e)

dihedral :: Int -> [Dihedral]
dihedral n = genFrom [rotateOn n, reflect]

d x = Di (False, base x)

r :: Integer -> Dihedral -> Dihedral
r n x = Di (False, n <+> e) .> x

s :: Dihedral -> Dihedral
s x = Di (True, e) .> x

