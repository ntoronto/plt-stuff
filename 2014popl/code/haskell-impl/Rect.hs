{-# LANGUAGE
    TypeFamilies #-}

module Rect where

import Set

data Rect s1 s2 = EmptyRect | UnivRect | Rect s1 s2
  deriving(Show,Eq)

prod :: (Set s1, Set s2) => s1 -> s2 -> Rect s1 s2
prod a1 a2 = if a1 == empty || a2 == empty then EmptyRect else Rect a1 a2

projFst :: (Set s1, Set s2) => Rect s1 s2 -> s1
projFst EmptyRect = empty
projFst (Rect a1 a2) = a1

projSnd :: (Set s1, Set s2) => Rect s1 s2 -> s2
projSnd EmptyRect = empty
projSnd (Rect a1 a2) = a2

instance (Set s1, Set s2) => Set (Rect s1 s2) where
  type MemberType (Rect s1 s2) = (MemberType s1, MemberType s2)

  empty = EmptyRect
  universe = UnivRect

  meet EmptyRect _ = EmptyRect
  meet _ EmptyRect = EmptyRect
  meet UnivRect a = a
  meet a UnivRect = a
  meet (Rect a1 a2) (Rect b1 b2) = prod (meet a1 b1) (meet a2 b2)

  join EmptyRect a = a
  join a EmptyRect = a
  join UnivRect _ = UnivRect
  join _ UnivRect = UnivRect
  join (Rect a1 a2) (Rect b1 b2) = prod (join a1 b1) (join a2 b2)

  contains EmptyRect _ = False
  contains UnivRect _ = True
  contains (Rect a1 a2) (b1,b2) = contains a1 b1 && contains a2 b2

  singleton (a1,a2) = Rect (singleton a1) (singleton a2)

