{-# LANGUAGE
    MultiParamTypeClasses #-}

module Set where

import GHC.Prim

class Container s x where
  contContains :: s x -> x -> Bool
  contSingleton :: x -> s x

class Container s x => Set s x where
  empty :: s x
  isEmpty :: s x -> Bool
  meet :: s x -> s x -> s x
  join :: s x -> s x -> s x

  contains :: s x -> x -> Bool
  contains = contContains

  singleton :: x -> s x
  singleton = contSingleton

