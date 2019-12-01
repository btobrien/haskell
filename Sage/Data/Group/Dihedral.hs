
module Data.Group.Dihedral where

import Utils
import Data.Group
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Group.Modulo
import Data.Group.Permutation

newtype Dihedral = Di (Bool,Modulo); val (Di x) = x

reflected :: Dihedral -> Bool
reflected = fst.val

xcompose :: (Bool,Modulo) -> (Bool,Modulo) -> (Bool,Modulo)
xcompose (a,b) (c,d) = if c
    then (a<>c, inv b <> d) 
    else (a, b<>d)

instance Show Dihedral where
    show x | x == e = "e"
    show x | snd (val x) == 0 = "s"
    show x |reflected x = 's':'r':(show.inv.snd.val $ x)
    show x  = 'r':(show.snd.val $ x)

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
    inv g = if reflected g
        then g
        else reflect <> g <> reflect

rotate n = Di (False, n)
reflect = Di (True, e)
dih n = (<>) (Di (False, base n 0))

dihedral :: Int -> [Dihedral]
dihedral n = gen [dih n (rotate 1), dih n reflect]

rotateOn :: Int -> Permutation Int
rotateOn n = P [[1..n]]

reflectOn :: Int -> Permutation Int
reflectOn n = reduce
    . map (\x -> [evaluate x, (evaluate.inv) x])
    . take (n`div`2) . tail
    $ modulo n

dihedralOn :: Int -> [Permutation Int]
dihedralOn n = gen [rotateOn n, reflectOn n]

