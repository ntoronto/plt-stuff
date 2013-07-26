{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances #-}

module Interval where

import GHC.Prim
import Set
import PMappable


class Corner x where
  lte :: x -> x -> Bool
  cmin :: x -> x -> x
  cmax :: x -> x -> x
  cmin a1 a2  =  if lte a1 a2 then a1 else a2
  cmax a1 a2  =  if lte a1 a2 then a2 else a1

instance Corner Float where
  lte a1 a2  =  a1 <= a2

instance Corner Integer where
  lte a1 a2  =  a1 <= a2

instance Corner Bool where
  lte a1 a2  =  a1 <= a2

instance (Pairable p, Corner x, Corner y) => Corner (p x y) where
  lte a1 a2  =  lte (pairFst a1) (pairFst a2) && lte (pairSnd a1) (pairSnd a2)


data Interval x  =  Ivl x x | Empty  deriving(Show,Eq)

ivl :: Corner x => x -> x -> Interval x
ivl a1 a2  =  if lte a1 a2 then Ivl a1 a2 else Empty


instance Set Interval where
  type SetCtxt x = Corner x

  empty  =  Empty

  isEmpty Empty  =  True
  isEmpty (Ivl a1 a2)  =  False

  meet (Ivl a1 a2) (Ivl b1 b2)  =  ivl (cmax a1 b1) (cmin a2 b2)
  meet Empty _  =  Empty
  meet _ Empty  =  Empty

  join (Ivl a1 a2) (Ivl b1 b2)  =  ivl (cmin a1 b1) (cmax a2 b2)
  join Empty a  =  a
  join a Empty  =  a

  contains (Ivl a1 a2) a  =  lte a1 a && lte a a2
  contains Empty a  =  False

  prod (Ivl a1 a2) (Ivl b1 b2)  =  ivl (pair a1 b1) (pair a2 b2)
  prod Empty _  =  Empty
  prod _ Empty  =  Empty

  projFst (Ivl a1 a2)  =  ivl (pairFst a1) (pairFst a2)
  projFst Empty  =  Empty

  projSnd (Ivl a1 a2)  =  ivl (pairSnd a1) (pairSnd a2)
  projSnd Empty  =  Empty

  singleton a  =  Ivl a a

instance PMappable Interval where
  type PMappableCtxt x = Corner x

