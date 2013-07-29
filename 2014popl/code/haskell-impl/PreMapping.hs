{-# LANGUAGE
    MultiParamTypeClasses,
    ConstraintKinds,
    TypeFamilies,
    FlexibleContexts #-}

module PreMapping where

import GHC.Prim
import Set
import Rect
import Pairable

data PreMapping s1 s2 x1 x2 = PreMapping { range :: s2 x2, preimage :: (s2 x2 -> s1 x1) }

preAp :: (Set s1 x, Set s2 y) => PreMapping s1 s2 x y -> s2 y -> s1 x
preAp (PreMapping ys p) bs  =  p (meet bs ys)

prePair :: (Pairable y, Set s1 x, Set s2 (Fst y), Set s3 (Snd y)) => PreMapping s1 s2 x (Fst y) -> PreMapping s1 s3 x (Snd y) -> PreMapping s1 (Rect s2 s3) x y
prePair (PreMapping ys py) (PreMapping zs pz)  =
  PreMapping (prod ys zs)
           (\ bc -> meet (py (projFst bc)) (pz (projSnd bc)))

preComp :: (Set s1 x, Set s2 y, Set s3 z) => PreMapping s2 s3 y z -> PreMapping s1 s2 x y -> PreMapping s1 s3 x z
preComp (PreMapping zs pz) hy  =  PreMapping zs (\ cs -> preAp hy (pz cs))

prePlus :: (Set s1 x, Set s2 y) => PreMapping s1 s2 x y -> PreMapping s1 s2 x y -> PreMapping s1 s2 x y
prePlus h1 h2  =
  PreMapping (join (range h1) (range h2))
           (\ bs -> join (preAp h1 bs) (preAp h2 bs))

