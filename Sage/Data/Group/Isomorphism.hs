

module Data.Group.Isomorphism where

import Utils
import Data.Group

classify :: Group a => [a] -> Maybe Class
classify = undefined

data Class = S Int | A Int | D Int | Z Int | X [Class]

