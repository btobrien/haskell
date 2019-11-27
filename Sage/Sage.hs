
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

(.>) :: Semigroup a => a -> a -> a
(.>) = flip (<>)

(<:) :: Ord a => Cycle a -> Permutation a -> Permutation a
c <: p = reduce [c] <> p

r n = flip (<>) (D.rotate n)
s = flip (<>) D.reflect 
dih n f =  f . dihbase n $ e

infixr 0 |. 
(|.) = ($)

--
infixr 0 ||| 
name ||| x = putStr (take 13 (name ++ repeat ' ')) >> print x

main = test 5

test :: Int -> IO ()
test n = do
	"bool"         |||  isGroup <$> [[True,False]]
	"modulos"      |||  isGroup . modulo <$> [2..n]
	"D.dihedrals"  |||  isGroup . D.dihedral <$> [2..n]
	"S.dihedrals"  |||  isGroup . S.dihedral <$> [2..n]
	"alternatings" |||  isGroup . alternating <$> [2..n]
	"symmetrics"   |||  isGroup . symmetric <$> [2..n]
