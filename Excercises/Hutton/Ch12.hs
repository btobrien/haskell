
import Control.Applicative 

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving Show


instance Functor Tree where
--  fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)


--instance Functor ((->) x) where
--    fmap :: (a -> b) -> (x -> a) -> (x -> b)
--    fmap = (.)

--instance Applicative ((->) x) where
--    pure :: a -> (x -> a)
--    pure = const
--    <*> :: (x -> (a -> b)) -> (x -> a) -> (x -> b)
--    f <*> g = \x -> (f x) (g x)


newtype ZippyList a = Z [a]
    deriving Show

instance Functor ZippyList where
    fmap f (Z xs) = Z $ map f xs

instance Applicative ZippyList where
--  pure :: a -> ZippyList a
    pure a = Z $ repeat a

--  <*> :: ZippyList (a -> b) -> ZippyList a -> ZippyList b
    (Z fs) <*> (Z xs) = Z $ zipWith ($) fs xs

-- pure id <*> x   = x
-- pure (g x)      = pure g <*> pure x <*>
-- x <*> pure y    = pure (\g -> g y) <*> x
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

--1 
-- x :: Monad a => a
-- 2
--
