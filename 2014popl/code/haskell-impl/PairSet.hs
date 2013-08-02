{-# LANGUAGE
    TypeFamilies #-}

module PairSet where

import Set

-- A type `PairSet s1 s2' denotes products of lattice subsets, and defines a lattice itself.
-- Members of these subsets are of type (MemberType s1, MemberType s2).

-- WARNING: Do not use the `PairSet' constructor! Use `prod' instead.
-- While `PairSet empty a' is technically empty, `PairSet empty a =/= empty', which messes up
-- emptiness checks in the preimage arrow implementation.
-- On the other hand, `prod empty a == empty'.

-- This could probably be enforced by the type system, but the implementation would be a less direct
-- translation of the approximating semantics.

data PairSet s1 s2 = EmptyPairSet | UnivPairSet | PairSet s1 s2
  deriving(Show,Eq)

-- The following functions could be put in a typeclass if it made sense to have more than one
-- product type

prod :: (Set s1, Set s2) => s1 -> s2 -> PairSet s1 s2
prod a1 a2 =
  if a1 == empty || a2 == empty
  then EmptyPairSet
  else if a1 == univ && a2 == univ
       then UnivPairSet
       else PairSet a1 a2

projFst :: (Set s1, Set s2) => PairSet s1 s2 -> s1
projFst EmptyPairSet = empty
projFst UnivPairSet = univ
projFst (PairSet a1 a2) = a1

projSnd :: (Set s1, Set s2) => PairSet s1 s2 -> s2
projSnd EmptyPairSet = empty
projSnd UnivPairSet = univ
projSnd (PairSet a1 a2) = a2

instance (Set s1, Set s2) => Set (PairSet s1 s2) where
  type MemberType (PairSet s1 s2) = (MemberType s1, MemberType s2)

  empty = EmptyPairSet
  univ = UnivPairSet

  EmptyPairSet /\ _ = EmptyPairSet
  _ /\ EmptyPairSet = EmptyPairSet
  UnivPairSet /\ a = a
  a /\ UnivPairSet = a
  PairSet a1 a2 /\ PairSet b1 b2 = prod (a1 /\ b1) (a2 /\ b2)

  EmptyPairSet \/ a = a
  a \/ EmptyPairSet = a
  UnivPairSet \/ _ = UnivPairSet
  _ \/ UnivPairSet = UnivPairSet
  PairSet a1 a2 \/ PairSet b1 b2 = prod (a1 \/ b1) (a2 \/ b2)

  member EmptyPairSet _ = False
  member UnivPairSet _ = True
  member (PairSet a1 a2) (b1,b2) = member a1 b1 && member a2 b2

  singleton (a1,a2) = prod (singleton a1) (singleton a2)

