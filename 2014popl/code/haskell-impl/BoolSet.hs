{-# LANGUAGE
    TypeFamilies #-}

module BoolSet where

import Set

data BoolSet = EmptyBoolSet | UnivBoolSet | TrueSet | FalseSet
  deriving(Show,Eq)

instance Set BoolSet where
  type MemberType BoolSet = Bool

  empty = EmptyBoolSet
  universe = UnivBoolSet

  meet EmptyBoolSet _ = EmptyBoolSet
  meet _ EmptyBoolSet = EmptyBoolSet
  meet UnivBoolSet a = a
  meet a UnivBoolSet = a
  meet a b = if a == b then a else EmptyBoolSet

  join EmptyBoolSet a = a
  join a EmptyBoolSet = a
  join UnivBoolSet _ = UnivBoolSet
  join _ UnivBoolSet = UnivBoolSet
  join a b = if a == b then a else UnivBoolSet

  contains EmptyBoolSet _ = False
  contains UnivBoolSet _ = True
  contains TrueSet True = True
  contains FalseSet False = True
  contains _ _ = False

  singleton True = TrueSet
  singleton False = FalseSet

