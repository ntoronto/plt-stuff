{-# LANGUAGE
    TypeFamilies,
    FlexibleContexts #-}

module AppxArrow where

import Set
import Rect
import BoolSet

infixr 1 ~>>>
infixr 3 ~&&&

class AppxArrow a where
  (~>>>) :: (Set s1, Set s2, Set s3)
            => a s1 s2 -> a s2 s3 -> a s1 s3

  (~&&&) :: (Set s1, Set s2, Set s3)
            => a s1 s2 -> a s1 s3 -> a s1 (Rect s2 s3)

  appxIfte :: (Set s1, Set s2)
              => a s1 BoolSet -> a s1 s2 -> a s1 s2 -> a s1 s2

  appxLazy :: (Set s1, Set s2) => a s1 s2 -> a s1 s2

  appxId :: Set s1 => a s1 s1
  appxConst :: (Set s1, Set s2) => (MemberType s2) -> a s1 s2
  appxFst :: (Set s1, Set s2) => a (Rect s1 s2) s1
  appxSnd :: (Set s1, Set s2) => a (Rect s1 s2) s2

