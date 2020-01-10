
import Utils
import Data.Group
import Data.Group.Dihedral
import System.Environment
import Control.Monad

main = do dumps . subgroups . dihedral . read . head =<< getArgs
