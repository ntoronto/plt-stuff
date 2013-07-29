module PreMapping where

import Set
import Rect

data PreMapping s1 s2 x y = PreMapping { range :: s2 y, preimage :: (s2 y -> s1 x) }

preAp :: (Set s1 x, Set s2 y) => PreMapping s1 s2 x y -> s2 y -> s1 x
preAp (PreMapping ys p) bs  =  p (meet bs ys)

prePair :: (Set s1 x, Set s2 y, Set s3 z)
           => PreMapping s1 s2 x y -> PreMapping s1 s3 x z -> PreMapping s1 (Rect s2 s3) x (y,z)
prePair (PreMapping ys py) (PreMapping zs pz)  =
  PreMapping (prod ys zs)
             (\ bc -> meet (py (projFst bc)) (pz (projSnd bc)))

preComp :: (Set s1 x, Set s2 y, Set s3 z)
           => PreMapping s2 s3 y z -> PreMapping s1 s2 x y -> PreMapping s1 s3 x z
preComp (PreMapping zs pz) hy  =  PreMapping zs (\ cs -> preAp hy (pz cs))

prePlus :: (Set s1 x, Set s2 y)
           => PreMapping s1 s2 x y -> PreMapping s1 s2 x y -> PreMapping s1 s2 x y
prePlus h1 h2  =
  PreMapping (join (range h1) (range h2))
             (\ bs -> join (preAp h1 bs) (preAp h2 bs))

