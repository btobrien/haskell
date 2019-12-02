
import Prelude hiding ((^))
import Utils
import Sage
import Control.Applicative
import Data.List
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

q2 = undefined

q3 = do
    __
    dumps $ cayleyTable (modulo 4)
    __
    dumpx $ cayleyTable (dihedral 4)
    __

q15 = check (dihedral 3) (hasOrderOf 6) isAbelian

q16 =
    dih 3 (s<>r)^2
    ==
    dih 3 s^2<>r^2

q17 = do
    print $ test (modulo 8)
    print $ test (dihedral 4)
    print $ test (thruples (modulo 2))
    where
    test xs = check xs (hasOrderOf 8) (length.subgroups)

q26 = all (any selfinv) $ map units [2..35]

q34 = do
    __
    dumps.subgroups $ pairs (modulo 3)
    __
    dumps.subgroups $ modulo 9
    __
    
q35 = dump $ subgroups (dihedral 3)

q36 = dump $ subgroups (dihedral 4)

q45 = all isSubgroup .
    mapp intersect . pairs $ subgroups (symmetric 3)

q46 = all isSubgroup .
    mapp union . pairs $ subgroups (symmetric 3)

q47 = all isSubgroup .
    mapp products . pairs $ subgroups (symmetric 3)

q52 = check (dihedral 3) (not.isAbelian) $
    all (not.isAbelian) . subgroups

