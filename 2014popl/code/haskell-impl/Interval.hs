{-# LANGUAGE
    FlexibleInstances,
    MultiParamTypeClasses #-}

module Interval where

import Set


data Interval x  =  EmptyIvl | Ivl x x  deriving(Show,Eq)

ivl :: Ord x => x -> x -> Interval x
ivl a1 a2  =  if a1 <= a2 then Ivl a1 a2 else EmptyIvl

instance Ord x => Set Interval x where
  empty  =  EmptyIvl

  isEmpty EmptyIvl  =  True
  isEmpty (Ivl a1 a2)  =  False

  meet EmptyIvl _  =  EmptyIvl
  meet _ EmptyIvl  =  EmptyIvl
  meet (Ivl a1 a2) (Ivl b1 b2)  =  ivl (max a1 b1) (min a2 b2)

  join EmptyIvl a  =  a
  join a EmptyIvl  =  a
  join (Ivl a1 a2) (Ivl b1 b2)  =  ivl (min a1 b1) (max a2 b2)

  contains EmptyIvl a  =  False
  contains (Ivl a1 a2) a  =  a1 <= a && a <= a2

  singleton a  =  Ivl a a

