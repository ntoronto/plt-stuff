{-# LANGUAGE
    TypeFamilies #-}

module Rect where

import Set

-- A type `Rect s1 s2' denotes products of lattice subsets, and defines a lattice itself.
-- Members of these subsets are of type (MemberType s1, MemberType s2).

-- WARNING: Do not use the `Rect' constructor! Use `prod' instead.
-- While `Rect empty a' is technically empty, `Rect empty a =/= empty', which messes up emptiness
-- checks in the preimage arrow implementation.
-- On the other hand, `prod empty a == empty'.

-- This could probably be enforced by the type system, but the implementation would be a less direct
-- translation of the approximating semantics.

data Rect s1 s2 = EmptyRect | UnivRect | Rect s1 s2
  deriving(Show,Eq)

-- The following functions could be put in a typeclass if it made sense to have more than one
-- product type

prod :: (Set s1, Set s2) => s1 -> s2 -> Rect s1 s2
prod a1 a2 =
  if a1 == empty || a2 == empty
  then EmptyRect
  else if a1 == universe && a2 == universe
       then UnivRect
       else Rect a1 a2

projFst :: (Set s1, Set s2) => Rect s1 s2 -> s1
projFst EmptyRect = empty
projFst UnivRect = universe
projFst (Rect a1 a2) = a1

projSnd :: (Set s1, Set s2) => Rect s1 s2 -> s2
projSnd EmptyRect = empty
projSnd UnivRect = universe
projSnd (Rect a1 a2) = a2

instance (Set s1, Set s2) => Set (Rect s1 s2) where
  type MemberType (Rect s1 s2) = (MemberType s1, MemberType s2)

  empty = EmptyRect
  universe = UnivRect

  EmptyRect /\ _ = EmptyRect
  _ /\ EmptyRect = EmptyRect
  UnivRect /\ a = a
  a /\ UnivRect = a
  Rect a1 a2 /\ Rect b1 b2 = prod (a1 /\ b1) (a2 /\ b2)

  EmptyRect \/ a = a
  a \/ EmptyRect = a
  UnivRect \/ _ = UnivRect
  _ \/ UnivRect = UnivRect
  Rect a1 a2 \/ Rect b1 b2 = prod (a1 \/ b1) (a2 \/ b2)

  contains EmptyRect _ = False
  contains UnivRect _ = True
  contains (Rect a1 a2) (b1,b2) = contains a1 b1 && contains a2 b2

  singleton (a1,a2) = prod (singleton a1) (singleton a2)

