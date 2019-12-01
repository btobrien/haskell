
module Sage where

import Data.Semigroup
import Utils
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Group
import Data.Group.Modulo
import Data.Group.Units
import Data.Group.Permutation
import Data.Group.Dihedral
import Data.Group.NonGroup
import Control.Applicative
--import Data.Group.Table

(.>) :: Semigroup a => a -> a -> a
(.>) = flip (<>)

(<:) :: Ord a => Cycle a -> Permutation a -> Permutation a
c <: p = reduce [c] <> p

r = rotate 1
s = reflect 

infixr 0 |. 
(|.) = ($)

--
infixr 0 ||| 
name ||| x = putStr (take 13 (name ++ repeat ' ')) >> print x

test :: Int -> IO ()
test n = do
    "bool"         |||  isGroup <$> [[True,False]]
    "modulos"      |||  isGroup . modulo <$> [2..n]
    "units"        |||  isGroup . units <$> [2..n]
    "dihedrals"    |||  isGroup . dihedral <$> [2..n]
    "dihedralOns"  |||  isGroup . dihedralOn <$> [2..n]
    "alternatings" |||  isGroup . alternating <$> [2..n]
    "symmetrics"   |||  isGroup . symmetric <$> [2..n]