{-# LANGUAGE
    StandaloneDeriving,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses #-}

module Rect where

import Set
import Pairable

newtype Rect s1 s2 x  =  Rect (s1 (Fst x), s2 (Snd x))

deriving instance (Set s1 x1, Set s2 x2, Show (s1 x1), Show (s2 x2)) => Show (Rect s1 s2 (x1,x2))
deriving instance (Set s1 x1, Set s2 x2, Eq (s1 x1), Eq (s2 x2)) => Eq (Rect s1 s2 (x1,x2))

prod :: (Set s1 x1, Set s2 x2) => s1 x1 -> s2 x2 -> Rect s1 s2 (x1,x2)
prod a1 a2 = Rect (a1,a2)

projFst :: (Set s1 x1, Set s2 x2) => Rect s1 s2 (x1,x2) -> s1 x1
projFst (Rect (a1,a2)) = a1

projSnd :: (Set s1 x1, Set s2 x2) => Rect s1 s2 (x1,x2) -> s2 x2
projSnd (Rect (a1,a2)) = a2

instance (Set s1 x1, Set s2 x2) => Set (Rect s1 s2) (x1,x2) where
  meet (Rect (a1,a2)) (Rect (b1,b2)) =
    do c1 <- meet a1 b1
       c2 <- meet a2 b2
       return (prod c1 c2)

  join (Rect (a1,a2)) (Rect (b1,b2)) = prod (join a1 b1) (join a2 b2)

  contains (Rect (a1,a2)) (b1,b2) = contains a1 b1 && contains a2 b2

  singleton (a1,a2) = Rect (singleton a1, singleton a2)

