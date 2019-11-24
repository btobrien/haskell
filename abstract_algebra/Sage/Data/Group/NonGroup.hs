
module Data.Group.NonGroup where

import Utils
import Data.Group

data NonGroup = NG { ng :: Int }
instance Show NonGroup where show (NG n) = show n
instance Eq NonGroup where (==) = (==) `on` ng
instance Ord NonGroup where compare = compare `on` ng
instance Semigroup NonGroup where
    (NG 0) <> (NG 0) = NG 0
    (NG 0) <> (NG 1) = NG 1
    (NG 0) <> (NG 2) = NG 2
    (NG 1) <> (NG 0) = NG 1
    (NG 1) <> (NG 1) = NG 1
    (NG 1) <> (NG 2) = NG 2
    (NG 2) <> (NG 0) = NG 2
    (NG 2) <> (NG 1) = NG 1
    (NG 2) <> (NG 2) = NG 2
instance Monoid NonGroup where mempty = NG 0
instance Group NonGroup where
    inv (NG 0) = NG 0
    inv (NG 1) = NG 2
    inv (NG 2) = NG 1

nonGroup = [NG 0, NG 1, NG 2]
