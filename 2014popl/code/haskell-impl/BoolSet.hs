{-# LANGUAGE
    TypeFamilies #-}

module BoolSet where

import Set

-- The type `BoolSet' denotes a set of booleans.

data BoolSet = EmptyBoolSet | UnivBoolSet | TrueSet | FalseSet
  deriving(Show,Eq)

instance Set BoolSet where
  type MemberType BoolSet = Bool

  empty = EmptyBoolSet
  universe = UnivBoolSet

  EmptyBoolSet /\ _ = EmptyBoolSet
  _ /\ EmptyBoolSet = EmptyBoolSet
  UnivBoolSet /\ a = a
  a /\ UnivBoolSet = a
  a /\ b = if a == b then a else EmptyBoolSet

  EmptyBoolSet \/ a = a
  a \/ EmptyBoolSet = a
  UnivBoolSet \/ _ = UnivBoolSet
  _ \/ UnivBoolSet = UnivBoolSet
  a \/ b = if a == b then a else UnivBoolSet

  contains EmptyBoolSet _ = False
  contains UnivBoolSet _ = True
  contains TrueSet True = True
  contains FalseSet False = True
  contains _ _ = False

  singleton True = TrueSet
  singleton False = FalseSet

