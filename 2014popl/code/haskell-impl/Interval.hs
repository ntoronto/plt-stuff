{-# LANGUAGE
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    MultiParamTypeClasses #-}

module Interval where

import Set


data Interval x  =  EmptyIvl | Ivl x x  deriving(Show,Eq)

ivl :: Ord x => x -> x -> Interval x
ivl a1 a2  =  if a1 <= a2 then Ivl a1 a2 else EmptyIvl

instance Ord x => Container Interval x where
  contContains (Ivl a1 a2) a  =  a1 <= a && a <= a2
  contContains EmptyIvl a  =  False

  contSingleton a  =  Ivl a a

instance (Ord x, Container Interval x) => Set Interval x where
  contains (Ivl a1 a2) a  =  a1 <= a && a <= a2
  contains EmptyIvl a  =  False

  singleton a  =  Ivl a a

  empty  =  EmptyIvl

  isEmpty EmptyIvl  =  True
  isEmpty (Ivl a1 a2)  =  False

  meet (Ivl a1 a2) (Ivl b1 b2)  =  ivl (max a1 b1) (min a2 b2)
  meet EmptyIvl _  =  EmptyIvl
  meet _ EmptyIvl  =  EmptyIvl

  join (Ivl a1 a2) (Ivl b1 b2)  =  ivl (min a1 b1) (max a2 b2)
  join EmptyIvl a  =  a
  join a EmptyIvl  =  a

