{-# LANGUAGE
    TypeFamilies #-}

module PreMapping where

import Set
import Rect

data PreMapping s1 s2 = PreMapping { preRange :: s2, runPreMapping :: s2 -> s1 }

preAp :: (Set s1, Set s2) => PreMapping s1 s2 -> s2 -> s1
preAp (PreMapping ys p) bs = p (meet ys bs)

prePair :: (Set s1, Set s2, Set s3)
           => PreMapping s1 s2 -> PreMapping s1 s3 -> PreMapping s1 (Rect s2 s3)
prePair (PreMapping ys py) (PreMapping zs pz) =
  PreMapping (prod ys zs) (\bcs -> meet (py (projFst bcs)) (pz (projSnd bcs)))

preComp :: (Set s1, Set s2, Set s3)
           => PreMapping s2 s3 -> PreMapping s1 s2 -> PreMapping s1 s3
preComp (PreMapping zs pz) hy =
  PreMapping zs (\cs -> do preAp hy (pz cs))

prePlus :: (Set s1, Set s2)
           => PreMapping s1 s2 -> PreMapping s1 s2 -> PreMapping s1 s2
prePlus h1 h2 =
  PreMapping (join (preRange h1) (preRange h2)) (\bs -> join (preAp h1 bs) (preAp h2 bs))

