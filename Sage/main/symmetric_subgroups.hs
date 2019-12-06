
import Utils
import Data.Group
import Data.Group.Permutation
import Data.Group.Dihedral

main = dumps . subgroups' . dihedral . read . head <<= getArgs
