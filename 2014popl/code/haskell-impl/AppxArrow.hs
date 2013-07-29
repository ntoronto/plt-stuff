{-# LANGUAGE
    ConstraintKinds,
    TypeFamilies,
    FlexibleContexts,
    MultiParamTypeClasses #-}

module AppxArrow where

import GHC.Prim
import Pairable
import Set
import Rect

infixr 1 ~>>>
infixr 3 ~&&&

class AppxArrow a where
  (~>>>) :: (Set s1 x, Set s2 y, Set s3 z)
            => a s1 s2 x y -> a s2 s3 y z -> a s1 s3 x z

  (~&&&) :: (Pairable y, Set s1 x, Set s2 (Fst y), Set s3 (Snd y))
            => a s1 s2 x (Fst y) -> a s1 s3 x (Snd y) -> a s1 (Rect s2 s3) x y

  appx_ifte :: (Set s1 x, Set s2 Bool, Set s3 y)
               => a s1 s2 x Bool -> a s1 s3 x y -> a s1 s3 x y -> a s1 s3 x y

  appx_id :: Set s1 x => a s1 s1 x x
  appx_const :: (Set s1 x, Set s2 y) => y -> a s1 s2 x y
  appx_fst :: (Pairable x, Set s1 (Fst x), Set s2 (Snd x)) => a (Rect s1 s2) s1 x (Fst x)
  appx_snd :: (Pairable x, Set s1 (Fst x), Set s2 (Snd x)) => a (Rect s1 s2) s2 x (Snd x)

