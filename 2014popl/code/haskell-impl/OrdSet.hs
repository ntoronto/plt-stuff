{-# LANGUAGE
    TypeFamilies #-}

module OrdSet where

import Set

-- A type `OrdSet x' denotes closed intervals of type `x' (with class `Ord x').

-- WARNING: Do not use the `Ivl' constructor! Use `ivl' instead; see PairSet.hs for reasons.

data OrdSet x = EmptyIvl | UnivIvl | Ivl x x
  deriving(Show,Eq)

ivl :: Ord x => x -> x -> OrdSet x
ivl a1 a2 = if a1 <= a2 then Ivl a1 a2 else EmptyIvl

instance Ord x => Set (OrdSet x) where
  type MemberType (OrdSet x) = x

  empty = EmptyIvl
  univ = UnivIvl

  EmptyIvl /\ _ = EmptyIvl
  _ /\ EmptyIvl = EmptyIvl
  UnivIvl /\ a = a
  a /\ UnivIvl = a
  Ivl a1 a2 /\ Ivl b1 b2 = ivl (max a1 b1) (min a2 b2)

  EmptyIvl \/ a = a
  a \/ EmptyIvl = a
  UnivIvl \/ _ = UnivIvl
  _ \/ UnivIvl = UnivIvl
  Ivl a1 a2 \/ Ivl b1 b2 = ivl (min a1 b1) (max a2 b2)

  member EmptyIvl _ = False
  member UnivIvl _ = True
  member (Ivl a1 a2) a = a1 <= a && a <= a2

  singleton a = ivl a a

