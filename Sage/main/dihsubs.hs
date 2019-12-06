
import Utils
import Data.Group
import Data.Group.Permutation
import Data.Group.Dihedral
import System.IO
import System.Environment
import Control.Monad

main = dumps . subgroups' . dihedral . read . head =<< getArgs
