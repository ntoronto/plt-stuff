{-# LANGUAGE
    MultiParamTypeClasses #-}

module Set where

class Set s x where
  meet :: s x -> s x -> Maybe (s x)
  join :: s x -> s x -> s x
  contains :: s x -> x -> Bool
  singleton :: x -> s x

