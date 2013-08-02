{-# LANGUAGE
    TypeFamilies #-}

module Interval where

import Set

-- A type `Interval x' denotes intervals of type `x' (with class `Ord x').

-- WARNING: Do not use the `Interval' constructor! Use `ivl' instead; see Rect.hs for reasons.

data Interval x = EmptyIvl | UnivIvl | Interval x x
  deriving(Show,Eq)

ivl :: Ord x => x -> x -> Interval x
ivl a1 a2 = if a1 <= a2 then Interval a1 a2 else EmptyIvl

instance Ord x => Set (Interval x) where
  type MemberType (Interval x) = x

  empty = EmptyIvl
  universe = UnivIvl

  EmptyIvl /\ _ = EmptyIvl
  _ /\ EmptyIvl = EmptyIvl
  UnivIvl /\ a = a
  a /\ UnivIvl = a
  Interval a1 a2 /\ Interval b1 b2 = ivl (max a1 b1) (min a2 b2)

  EmptyIvl \/ a = a
  a \/ EmptyIvl = a
  UnivIvl \/ _ = UnivIvl
  _ \/ UnivIvl = UnivIvl
  Interval a1 a2 \/ Interval b1 b2 = ivl (min a1 b1) (max a2 b2)

  contains EmptyIvl _ = False
  contains UnivIvl _ = True
  contains (Interval a1 a2) a = a1 <= a && a <= a2

  singleton a = ivl a a

