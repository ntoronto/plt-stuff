{-# LANGUAGE
    TypeFamilies #-}

module MaybeSet where

import Set

data MaybeSet s = OnlyJust s | WithNothing s
  deriving(Show,Eq)

withoutNothing :: Set s => MaybeSet s -> s
withoutNothing (OnlyJust a) = a
withoutNothing (WithNothing a) = a

instance Set s => Set (MaybeSet s) where
  type MemberType (MaybeSet s) = Maybe (MemberType s)

  empty = OnlyJust empty
  universe = WithNothing universe

  meet (OnlyJust a) (OnlyJust b) = OnlyJust (meet a b)
  meet (OnlyJust a) (WithNothing b) = OnlyJust (meet a b)
  meet (WithNothing a) (OnlyJust b) = OnlyJust (meet a b)
  meet (WithNothing a) (WithNothing b) = WithNothing (meet a b)

  join (OnlyJust a) (OnlyJust b) = OnlyJust (join a b)
  join (WithNothing a) (OnlyJust b) = WithNothing (join a b)
  join (OnlyJust a) (WithNothing b) = WithNothing (join a b)
  join (WithNothing a) (WithNothing b) = WithNothing (join a b)

  contains (WithNothing a) Nothing = True
  contains (OnlyJust a) Nothing = False
  contains (WithNothing a) (Just x) = contains a x
  contains (OnlyJust a) (Just x) = contains a x

  singleton Nothing = WithNothing empty
  singleton (Just a) = OnlyJust (singleton a)

