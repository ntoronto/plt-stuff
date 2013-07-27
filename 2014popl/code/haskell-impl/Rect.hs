{-# LANGUAGE
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    StandaloneDeriving,
    MultiParamTypeClasses #-}

module Rect where

import Set
import Pairable

data Rect s1 s2 x  =  EmptyRect | Rect (s1 (Fst x)) (s2 (Snd x))

deriving instance (Pairable x, Show (s1 (Fst x)), Show (s2 (Snd x))) => Show (Rect s1 s2 x)
deriving instance (Pairable x, Eq (s1 (Fst x)), Eq (s2 (Snd x))) => Eq (Rect s1 s2 x)

instance Pairable x => Pairable (Rect s1 s2 x) where
  type Fst (Rect s1 s2 x) = Fst x
  type Snd (Rect s1 s2 x) = Snd x
  -- Can't implement pairFst and pairSnd: get an occurrence check error, which has since
  -- been fixed in 7.4.2
  --pairFst (Rect x1 x2) = x1
  --pairSnd (Rect x1 x2) = x2

instance (Pairable x, Container s1 (Fst x), Container s2 (Snd x)) => Container (Rect s1 s2) x where
  contContains EmptyRect _ = False
  contContains (Rect a1 a2) x = contContains a1 (pairFst x) && contContains a2 (pairSnd x)

  contSingleton x = Rect (contSingleton (pairFst x)) (contSingleton (pairSnd x))

instance (Pairable x, Set s1 (Fst x), Set s2 (Snd x)) => Set (Rect s1 s2) x where
  empty = EmptyRect

  isEmpty EmptyRect = True
  isEmpty (Rect _ _) = False

  meet EmptyRect _ = EmptyRect
  meet _ EmptyRect = EmptyRect
  meet (Rect a1 a2) (Rect b1 b2) = prod (meet a1 b1) (meet a2 b2)

  join EmptyRect b = b
  join a EmptyRect = a
  join (Rect a1 a2) (Rect b1 b2) = prod (join a1 b1) (join a2 b2)

class (Pairable x, Set s1 (Fst x), Set s2 (Snd x), Set (s s1 s2) x) => ProdSet s s1 s2 x where
  prod :: s1 (Fst x) -> s2 (Snd x) -> s s1 s2 x
  projFst :: s s1 s2 x -> s1 (Fst x)
  projSnd :: s s1 s2 x -> s2 (Snd x)

instance (Pairable x, Set s1 (Fst x), Set s2 (Snd x)) => ProdSet Rect s1 s2 x where
  prod a1 a2 = if (isEmpty a1 || isEmpty a2) then EmptyRect else Rect a1 a2

  projFst EmptyRect = empty
  projFst (Rect a1 a2) = a1

  projSnd EmptyRect = empty
  projSnd (Rect a1 a2) = a2

