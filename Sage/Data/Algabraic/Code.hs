
module Data.Algabraic.Code where

type Mode = (String -> String)
type Code = (Mode,Mode); encode = fst; decode = snd
