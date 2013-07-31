{-# LANGUAGE
    FlexibleInstances,
    MultiParamTypeClasses #-}

module Interval where

import Set

data Interval x = Ivl x x
  deriving(Show,Eq)

unitIvl :: Interval Float
unitIvl = Ivl 0.0 1.0

ivl :: Ord x => x -> x -> Maybe (Interval x)
ivl a1 a2 = if a1 <= a2 then Just (Ivl a1 a2) else Nothing

instance Ord x => Set Interval x where
  meet (Ivl a1 a2) (Ivl b1 b2) = ivl (max a1 b1) (min a2 b2)
  join (Ivl a1 a2) (Ivl b1 b2) = Ivl (min a1 b1) (max a2 b2)
  contains (Ivl a1 a2) a = a1 <= a && a <= a2
  singleton a = Ivl a a

