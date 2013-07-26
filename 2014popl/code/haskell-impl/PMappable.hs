{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

module PMappable where

import GHC.Prim
import Set

data PMapping s x y = PMapping { range :: (s y), preimage :: (s y -> s x) }


type Ctxt x = (PMappableCtxt x, SetCtxt x)

class Set s => PMappable s where
  type PMappableCtxt v :: Constraint

  preAp :: (Ctxt x, Ctxt y) => PMapping s x y -> s y -> s x
  preAp (PMapping ys p) bs  =  p (meet bs ys)

  prePair :: (Ctxt x, Ctxt y, Ctxt z) => PMapping s x y -> PMapping s x z -> PMapping s x (y,z)
  prePair (PMapping ys py) (PMapping zs pz)  =
    PMapping (prod ys zs)
             (\ bc -> meet (py (projFst bc)) (pz (projSnd bc)))

  preComp :: (Ctxt x, Ctxt y, Ctxt z) => PMapping s y z -> PMapping s x y -> PMapping s x z
  preComp (PMapping zs pz) hy  =  PMapping zs (\ cs -> preAp hy (pz cs))

  prePlus :: (Ctxt x, Ctxt y) => PMapping s x y -> PMapping s x y -> PMapping s x y
  prePlus h1 h2  =
    PMapping (join (range h1) (range h2))
             (\ bs -> join (preAp h1 bs) (preAp h2 bs))

