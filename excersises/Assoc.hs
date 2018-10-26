module Assoc where

type Assoc k v = [(k,v)]

find :: Eq k => (Assoc k v) -> k -> [v]
find t k = [ v | (k',v) <- t, k' == k ]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)
