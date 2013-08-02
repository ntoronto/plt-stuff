{-# LANGUAGE
    TypeFamilies #-}

module Interval where

import Set

data Interval x = EmptyIvl | UnivIvl | Ivl x x
  deriving(Show,Eq)

unitIvl :: Interval Float
unitIvl = Ivl 0.0 1.0

ivl :: Ord x => x -> x -> Interval x
ivl a1 a2 = if a1 <= a2 then Ivl a1 a2 else EmptyIvl

instance Ord x => Set (Interval x) where
  type MemberType (Interval x) = x

  empty = EmptyIvl
  universe = UnivIvl

  meet EmptyIvl _ = EmptyIvl
  meet _ EmptyIvl = EmptyIvl
  meet UnivIvl a = a
  meet a UnivIvl = a
  meet (Ivl a1 a2) (Ivl b1 b2) = ivl (max a1 b1) (min a2 b2)

  join EmptyIvl a = a
  join a EmptyIvl = a
  join UnivIvl _ = UnivIvl
  join _ UnivIvl = UnivIvl
  join (Ivl a1 a2) (Ivl b1 b2) = Ivl (min a1 b1) (max a2 b2)

  contains EmptyIvl _ = False
  contains UnivIvl _ = True
  contains (Ivl a1 a2) a = a1 <= a && a <= a2

  singleton a = Ivl a a

