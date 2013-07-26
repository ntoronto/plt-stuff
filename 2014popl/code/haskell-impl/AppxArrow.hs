{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

module AppxArrow where

import GHC.Prim

infixr 1 >>>
infixr 3 &&&

class AppxArrow a where
  type AppxArrowCtxt v :: Constraint
  (>>>) :: (AppxArrowCtxt x, AppxArrowCtxt y, AppxArrowCtxt z) => a x y -> a y z -> a x z
  (&&&) :: (AppxArrowCtxt x, AppxArrowCtxt y, AppxArrowCtxt z, AppxArrowCtxt (y,z)) => a x y -> a x z -> a x (y,z)
  ifte :: (AppxArrowCtxt Bool, AppxArrowCtxt x, AppxArrowCtxt y) => a x Bool -> a x y -> a x y -> a x y
  id :: AppxArrowCtxt x => a x x
  const :: (AppxArrowCtxt x, AppxArrowCtxt y) => y -> a x y
  fst :: (AppxArrowCtxt (x,y), AppxArrowCtxt x, AppxArrowCtxt y) => a (x,y) x
  snd :: (AppxArrowCtxt (x,y), AppxArrowCtxt x, AppxArrowCtxt y) => a (x,y) y

