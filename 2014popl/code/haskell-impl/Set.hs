{-# LANGUAGE
    MultiParamTypeClasses #-}

module Set where

class Set s x where
  empty :: s x
  isEmpty :: s x -> Bool
  meet :: s x -> s x -> s x
  join :: s x -> s x -> s x
  contains :: s x -> x -> Bool
  singleton :: x -> s x

