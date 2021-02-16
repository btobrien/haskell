
module Data.Group.Dihedral where

import Utils hiding (rotate)
import Data.Group
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Group.Modulo
import Data.Group.Permutation

newtype Dihedral = Di (Bool,Modulo); val (Di x) = x

reflected :: Dihedral -> Bool
reflected = fst.val

degree :: Dihedral -> Modulo
degree = snd.val

xcompose :: (Bool,Modulo) -> (Bool,Modulo) -> (Bool,Modulo)
xcompose (a,b) (c,d) = if c
    then (a<>c, inverse b <> d) 
    else (a, b<>d)

instance Show Dihedral where
    show x | x == identity = "e"
    show x = shows (reflected x) ++ showr (degree x)
        where
        shows s = if s
            then "s"
            else ""
        showr r = if r==0
            then ""
            else if r==1
                then "r"
                else 'r': show r

instance Eq Dihedral where
    (==) = (==) `on` val

instance Ord Dihedral where
    compare = compare `on` val

instance Semigroup Dihedral where
    (<>) = Di .: xcompose `on` val

instance Monoid Dihedral where
    mempty = Di mempty
    mappend = (<>)

instance Group Dihedral where
    inverse g = if reflected g
        then g
        else reflect <> g <> reflect

rotate n = Di (False, n)
reflect = Di (True, identity)
dih n = (<>) (Di (False, modulo (toInteger n) 0))

dihedral :: Integer -> [Dihedral]
dihedral n = gen [dih n (rotate 1), dih n reflect]

rotateOn :: Integer -> Permutation Integer
rotateOn n = P [[1..n]]

reflectOn :: Integer -> Permutation Integer
reflectOn n = reduce
    . map (\x -> [toInteger x, (toInteger.inverse) x])
    . take (fromInteger n`div`2)
    $ modulos n

dihedralOn :: Integer -> [Permutation Integer]
dihedralOn n = gen [rotateOn n, reflectOn n]

