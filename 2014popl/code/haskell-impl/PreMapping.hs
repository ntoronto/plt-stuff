module PreMapping where

import Set
import Rect

data PreMapping s1 s2 x y = PreMapping { range :: s2 y, preimage :: s2 y -> Maybe (s1 x) }

preAp :: (Set s1 x, Set s2 y) => Maybe (PreMapping s1 s2 x y) -> s2 y -> Maybe (s1 x)
preAp h bs =
  do PreMapping ys p <- h
     p =<< meet bs ys

prePair :: (Set s1 x, Set s2 y, Set s3 z)
           => Maybe (PreMapping s1 s2 x y) -> Maybe (PreMapping s1 s3 x z) -> Maybe (PreMapping s1 (Rect s2 s3) x (y,z))
prePair h1 h2 =
  do PreMapping ys py <- h1
     PreMapping zs pz <- h2
     return (PreMapping (prod ys zs)
                        (\ bc -> do xs1 <- py (projFst bc)
                                    xs2 <- pz (projSnd bc)
                                    meet xs1 xs2))
     
preComp :: (Set s1 x, Set s2 y, Set s3 z)
           => Maybe (PreMapping s2 s3 y z) -> Maybe (PreMapping s1 s2 x y) -> Maybe (PreMapping s1 s3 x z)
preComp hz hy =
  do PreMapping zs pz <- hz
     return (PreMapping zs (\ cs -> do ys <- pz cs
                                       preAp hy ys))

-- This is WRONG: if one of them is Nothing, the other should get returned
prePlus :: (Set s1 x, Set s2 y)
           => Maybe (PreMapping s1 s2 x y) -> Maybe (PreMapping s1 s2 x y) -> Maybe (PreMapping s1 s2 x y)
prePlus h1 h2 =
  do PreMapping ys1 p1 <- h1
     PreMapping ys2 p2 <- h2
     return (PreMapping (join ys1 ys2)
                        (\ bs -> do xs1 <- preAp h1 bs
                                    xs2 <- preAp h2 bs
                                    return (join xs1 xs2)))

