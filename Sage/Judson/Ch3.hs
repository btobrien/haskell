
import Prelude hiding ((^))
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


-- TODO: support CayleyTable group implementation?
a2 = undefined

a3 = do
    print "Z4"
    dumps $ cayleyTable (modulo 4)
    print "D4"
    dumps $ cayleyTable (D.dihedral 4)

a15 = do
    print.length $ symmetric 3
    print.isAbelian $ symmetric 3

a16 =
    dih 3 (s<>r)^2
    ==
    dih 3 s^2<>r^2

a17 = do
    print.size $ modulo 8
    print.size $ dihedral 4
    print.size $ (,,) <$>
            modulo 2 <*>
            modulo 2 <*>
            modulo 2
    where
    size xs = (length xs, length (subgroups xs))

a26 = undefined
