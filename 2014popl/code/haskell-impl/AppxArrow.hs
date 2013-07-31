{-# LANGUAGE
    FlexibleContexts #-}

module AppxArrow where

import Set
import Rect

infixr 1 ~>>>
infixr 3 ~&&&

class AppxArrow a where
  (~>>>) :: (Set s1 x, Set s2 y, Set s3 z)
            => a s1 s2 x y -> a s2 s3 y z -> a s1 s3 x z

  (~&&&) :: (Set s1 x, Set s2 y, Set s3 z)
            => a s1 s2 x y -> a s1 s3 x z -> a s1 (Rect s2 s3) x (y,z)

  appxIfte :: (Set s1 x, Set s2 Bool, Set s3 y)
               => a s1 s2 x Bool -> a s1 s3 x y -> a s1 s3 x y -> a s1 s3 x y

  appxId :: Set s1 x => a s1 s1 x x
  appxConst :: (Set s1 x, Set s2 y) => y -> a s1 s2 x y
  appxFst :: (Set s1 x1, Set s2 x2) => a (Rect s1 s2) s1 (x1,x2) x1
  appxSnd :: (Set s1 x1, Set s2 x2) => a (Rect s1 s2) s2 (x1,x2) x2

