
import Prelude hiding ((^))
import Utils
import Sage
import Control.Applicative
import Data.Semigroup
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Group
import Data.Group.Modulo
import Data.Group.Permutation
import Data.Group.Dihedral
import Data.Group.NonGroup
import Data.Group.Units
--import Data.Group.Table

a2 = undefined

a3 = do
    __
    ___ "caylet table Z4"
    dumps $ cayleyTable (modulo 4)
    __
    ___ "caylet table D4"
    dumpx $ cayleyTable (dihedral 4)
    __

a15 = do
    print.length $ symmetric 3
    print.isAbelian $ symmetric 3

a16 =
    dih 3 (s<>r)^2
    ==
    dih 3 s^2<>r^2

a17 = do
    ___ "(order, #subgroups)"
    print.size $ modulo 8
    print.size $ dihedral 4
    print.size $ thruples (modulo 2)
    where
    size xs = (length xs, length (subgroups xs))

a26 = all (any selfinv) $ map units [2..35]

a34 = do
    __
    ___ "subgroups Z3 x Z3"
    dumps.subgroups $ pairs (modulo 3)
    __
    ___ "subgroups Z9"
    dumps.subgroups $ modulo 9
    __
    
a35 = dump $ subgroups (dihedral 3)

