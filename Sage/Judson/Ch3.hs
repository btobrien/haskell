
import Utils
import Sage
import Data.Semigroup
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Group
import Data.Group.Modulo
import Data.Group.Permutation as S
import Data.Group.Dihedral as D
import Data.Group.NonGroup
import Control.Applicative
--import Data.Group.Table


-- TODO: support CayleyTable group implementation
a2 = undefined

q3 =
    "Write out Cayley tables for the groups formed by " ++
    "Z4 and D4. Are the they the same?"
a3 = do
    print "Z4"
    dumps $ cayleyTable (modulo 4)
    print "D4"
    dumps $ cayleyTable (D.dihedral 4)

q15 =
    "Prove or disprove that every group containing " ++
    "6 elements is abelian"
a15 = isAbelian (D.dihedral 3)
