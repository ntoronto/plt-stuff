{-# LANGUAGE
    TypeFamilies #-}

module MaybeSet where

import Set

-- A type `Set (MaybeSet s)' denotes the same subsets as a type of class `Set s', except the value
-- `Nothing' may be contained in any of them.

data MaybeSet s = OnlyJust s | WithNothing s
  deriving(Show,Eq)

withoutNothing :: Set s => MaybeSet s -> s
withoutNothing (OnlyJust a) = a
withoutNothing (WithNothing a) = a

instance Set s => Set (MaybeSet s) where
  type MemberType (MaybeSet s) = Maybe (MemberType s)

  empty = OnlyJust empty
  universe = WithNothing universe

  OnlyJust a /\ OnlyJust b = OnlyJust (a /\ b)
  OnlyJust a /\ WithNothing b = OnlyJust (a /\ b)
  WithNothing a /\ OnlyJust b = OnlyJust (a /\ b)
  WithNothing a /\ WithNothing b = WithNothing (a /\ b)

  OnlyJust a \/ OnlyJust b = OnlyJust (a \/ b)
  WithNothing a \/ OnlyJust b = WithNothing (a \/ b)
  OnlyJust a \/ WithNothing b = WithNothing (a \/ b)
  WithNothing a \/ WithNothing b = WithNothing (a \/ b)

  contains (WithNothing a) Nothing = True
  contains (OnlyJust a) Nothing = False
  contains (WithNothing a) (Just x) = contains a x
  contains (OnlyJust a) (Just x) = contains a x

  singleton Nothing = WithNothing empty
  singleton (Just a) = OnlyJust (singleton a)

