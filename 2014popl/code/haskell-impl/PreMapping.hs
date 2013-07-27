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

data PreMapping x1 x2 = PreMapping { range :: x2, preimage :: (x2 -> x1) }

preAp :: (Set s1 x, Set s2 y) => PreMapping (s1 x) (s2 y) -> s2 y -> s1 x
preAp (PreMapping ys p) bs  =  p (meet bs ys)

prePair :: (Pairable y, Set s1 x, Set s2 (Fst y), Set s3 (Snd y)) => PreMapping (s1 x) (s2 (Fst y)) -> PreMapping (s1 x) (s3 (Snd y)) -> PreMapping (s1 x) (Rect s2 s3 y)
prePair (PreMapping ys py) (PreMapping zs pz)  =
  PreMapping (prod ys zs)
           (\ bc -> meet (py (projFst bc)) (pz (projSnd bc)))

preComp :: (Set s1 x, Set s2 y, Set s3 z) => PreMapping (s2 y) (s3 z) -> PreMapping (s1 x) (s2 y) -> PreMapping (s1 x) (s3 z)
preComp (PreMapping zs pz) hy  =  PreMapping zs (\ cs -> preAp hy (pz cs))

prePlus :: (Set s1 x, Set s2 y) => PreMapping (s1 x) (s2 y) -> PreMapping (s1 x) (s2 y) -> PreMapping (s1 x) (s2 y)
prePlus h1 h2  =
  PreMapping (join (range h1) (range h2))
           (\ bs -> join (preAp h1 bs) (preAp h2 bs))

