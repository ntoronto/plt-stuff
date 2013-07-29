{-# LANGUAGE
    StandaloneDeriving,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses #-}

module Rect where

import Set
import Pairable

data Rect s1 s2 x  =  EmptyRect | Rect (s1 (Fst x)) (s2 (Snd x))

deriving instance (Set s1 x1, Set s2 x2, Show (s1 x1), Show (s2 x2)) => Show (Rect s1 s2 (x1,x2))
deriving instance (Set s1 x1, Set s2 x2, Eq (s1 x1), Eq (s2 x2)) => Eq (Rect s1 s2 (x1,x2))

prod :: (Set s1 x1, Set s2 x2) => s1 x1 -> s2 x2 -> Rect s1 s2 (x1,x2)
prod a1 a2 = if (isEmpty a1 || isEmpty a2) then EmptyRect else Rect a1 a2

projFst :: (Set s1 x1, Set s2 x2) => Rect s1 s2 (x1,x2) -> s1 x1
projFst EmptyRect = empty
projFst (Rect a1 a2) = a1

projSnd :: (Set s1 x1, Set s2 x2) => Rect s1 s2 (x1,x2) -> s2 x2
projSnd EmptyRect = empty
projSnd (Rect a1 a2) = a2

instance (Set s1 x1, Set s2 x2) => Set (Rect s1 s2) (x1,x2) where
  empty = EmptyRect

  isEmpty EmptyRect = True
  isEmpty (Rect _ _) = False

  meet EmptyRect _ = EmptyRect
  meet _ EmptyRect = EmptyRect
  meet (Rect a1 a2) (Rect b1 b2) = prod (meet a1 b1) (meet a2 b2)

  join EmptyRect b = b
  join a EmptyRect = a
  join (Rect a1 a2) (Rect b1 b2) = prod (join a1 b1) (join a2 b2)

  contains EmptyRect _ = False
  contains (Rect a1 a2) (b1,b2) = contains a1 b1 && contains a2 b2

  singleton (a1,a2) = Rect (singleton a1) (singleton a2)

