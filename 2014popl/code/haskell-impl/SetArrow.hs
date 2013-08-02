module SetArrow where

import Set
import PairSet
import BoolSet

infixr 1 ~>>>
infixr 3 ~&&&

class SetArrow a where
  (~>>>) :: (Set s1, Set s2, Set s3)
            => a s1 s2 -> a s2 s3 -> a s1 s3

  (~&&&) :: (Set s1, Set s2, Set s3)
            => a s1 s2 -> a s1 s3 -> a s1 (PairSet s2 s3)

  setIfte :: (Set s1, Set s2)
             => a s1 BoolSet -> a s1 s2 -> a s1 s2 -> a s1 s2

  setLazy :: (Set s1, Set s2) => a s1 s2 -> a s1 s2
  setId :: Set s1 => a s1 s1
  setConst :: (Set s1, Set s2) => (MemberType s2) -> a s1 s2
  setFst :: (Set s1, Set s2) => a (PairSet s1 s2) s1
  setSnd :: (Set s1, Set s2) => a (PairSet s1 s2) s2

