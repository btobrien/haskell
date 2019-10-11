

import Control.Monad.State
import Data.Tuple
import Prelude hiding (gcd)

-- interestingly, will automatically flip order if b > a
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a`mod`b)

euclidean :: Int -> Int -> (Int,Int)
euclidean a b = euclidean' (a,b) (1,0) (0,1)

euclidean' :: (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int)
euclidean' base a b =
    if dot base b == 0 then a
    else euclidean' base b r
    where
    r = a `plus` scale (-q) b
    q = div (dot base a) (dot base b)

dot :: (Int,Int) -> (Int,Int) -> Int
dot (a,b) (r,s) = (a*r) + (b*s)

plus :: (Int,Int) -> (Int,Int) -> (Int,Int)
plus (a,b) (r,s) = (a+r,b+s)

scale :: Int -> (Int,Int) -> (Int,Int)
scale k (a,b) = (k*a,k*b)
