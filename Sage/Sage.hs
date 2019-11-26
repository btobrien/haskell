
import Prelude hiding ((.))
import Data.Semigroup
import Utils
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Group
import Data.Group.Modulo
import Data.Group.Permutation as S
import Data.Group.Dihedral as D
import Data.Group.NonGroup
import Control.Applicative
--import Data.Group.Table
--

(.) :: Semigroup a => a -> a -> a
(.) = (<>)

(.:) :: Semigroup a => a -> a -> a
(.:) = flip (<>)

(<:) :: Ord a => Cycle a -> Permutation a -> Permutation a
c <: p = reduce [c] <> p

(|:) :: Show a => String -> a -> IO ()
name |: x = putStr (take 13 (name ++ repeat ' ')) >> print x

main = test 5
test n = do
	"bool"         |: [isGroup [True,False]]
	"modulos"      |: [isGroup (modulo n)     | n <- [2..n]]
	"D.dihedrals"  |: [isGroup (D.dihedral n)  | n <- [2..n]]
	"S.dihedrals"  |: [isGroup (S.dihedral n)  | n <- [2..n]]
	"alternatings" |: [isGroup (alternating n) | n <- [2..n]]
	"symmetrics"   |: [isGroup (symmetric n)   | n <- [2..n]]
