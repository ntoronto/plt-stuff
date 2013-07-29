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

  appx_ifte :: (Set s1 x, Set s2 Bool, Set s3 y)
               => a s1 s2 x Bool -> a s1 s3 x y -> a s1 s3 x y -> a s1 s3 x y

  appx_id :: Set s1 x => a s1 s1 x x
  appx_const :: (Set s1 x, Set s2 y) => y -> a s1 s2 x y
  appx_fst :: (Set s1 x1, Set s2 x2) => a (Rect s1 s2) s1 (x1,x2) x1
  appx_snd :: (Set s1 x1, Set s2 x2) => a (Rect s1 s2) s2 (x1,x2) x2

