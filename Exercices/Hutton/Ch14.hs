
-- Foldables & Friends: common patterns for  processing the values in a data structure

-- 14.1 Monoids

-- In mathematics, a moind is a set together with an associative binary operator and an identity element for the operator.

-- class Monoid a where
    -- mempty :: a
    -- mappend :: a -> a -> a
    --
    -- mconcat :: [a] -> a
    -- mconcat = foldr mappend mempty

-- Monoid Laws:
    -- mempty `mappend` x = x
    -- x `mappend` mempty = x
    -- x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z

-- x <> y = x `mappend` y

-- One of the primary applications of monoids in Haskell is to combine all the values is a data structure to give a single value.

-- 14.2 Foldables

-- why might foldl be more performant?? (see Ch.15...)
--
-- 14.4 remarks
-- there are two standard ways to genralize foldr from lists to other data structures, known in the literature as a catamorphisms and crush operators.











