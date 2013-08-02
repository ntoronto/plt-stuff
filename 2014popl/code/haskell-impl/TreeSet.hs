{-# LANGUAGE
    TypeFamilies,
    StandaloneDeriving,
    FlexibleInstances,
    MultiParamTypeClasses #-}

module TreeSet where

import Set
import Interval
import MaybeSet
import BoolSet


type TreeIndex = [Bool]

indexLeft :: TreeIndex -> TreeIndex
indexLeft j = True : j

indexRight :: TreeIndex -> TreeIndex
indexRight j = False : j

j0 = []


data TreeSet s = EmptyTreeSet | UnivTreeSet | TreeSetNode s !(TreeSet s) !(TreeSet s)
  deriving(Show,Eq)

data TreeVal x = AnyTreeVal | TreeValNode x !(TreeVal x) !(TreeVal x)
  deriving(Show,Eq)

treeSetNode :: Set s => s -> TreeSet s -> TreeSet s -> TreeSet s
treeSetNode x l r = if x == empty || l == empty || r == empty
                      then EmptyTreeSet
                      else TreeSetNode x l r

class Set s => TreeAxisSet s where
  fullAxis :: s

instance Set s => Set (TreeSet s) where
  type MemberType (TreeSet s) = TreeVal (MemberType s)

  empty = EmptyTreeSet
  universe = UnivTreeSet

  meet EmptyTreeSet _ = EmptyTreeSet
  meet _ EmptyTreeSet = EmptyTreeSet
  meet UnivTreeSet a = a
  meet a UnivTreeSet = a
  meet (TreeSetNode xs1 l1 r1) (TreeSetNode xs2 l2 r2) =
    TreeSetNode (meet xs1 xs2) (meet l1 l2) (meet r1 r2)

  join EmptyTreeSet a = a
  join a EmptyTreeSet = a
  join UnivTreeSet _ = UnivTreeSet
  join _ UnivTreeSet = UnivTreeSet
  join (TreeSetNode xs1 ls1 rs1) (TreeSetNode xs2 ls2 rs2) =
    TreeSetNode (join xs1 xs2) (join ls1 ls2) (join rs1 rs2)

  contains EmptyTreeSet _ = False
  contains UnivTreeSet _ = True
  contains (TreeSetNode xs ls rs) (TreeValNode x l r) =
    contains xs x && contains ls l && contains rs r

  singleton AnyTreeVal = UnivTreeSet
  singleton (TreeValNode x l r) = TreeSetNode (singleton x) (singleton l) (singleton r)

project :: TreeAxisSet s => TreeIndex -> TreeSet s -> s
project j EmptyTreeSet = empty
project j UnivTreeSet = fullAxis
project [] (TreeSetNode xs _ _) = xs
project  (True:j) (TreeSetNode _ l _) = project j l
project (False:j) (TreeSetNode _ _ r) = project j r

unproject :: TreeAxisSet s => TreeIndex -> TreeSet s -> s -> TreeSet s
unproject j UnivTreeSet ys = unproject j (TreeSetNode fullAxis UnivTreeSet UnivTreeSet) ys
unproject [] (TreeSetNode xs ls rs) ys = TreeSetNode (meet xs ys) ls rs
unproject  (True:j) (TreeSetNode xs ls rs) ys = TreeSetNode xs (unproject j ls ys) rs
unproject (False:j) (TreeSetNode xs ls rs) ys = TreeSetNode xs ls (unproject j rs ys)


type RSet = TreeSet (Interval Float)
type TSet = TreeSet (MaybeSet BoolSet)

instance TreeAxisSet (Interval Float) where
  fullAxis = Ivl 0.0 1.0

instance TreeAxisSet (MaybeSet BoolSet) where
  fullAxis = WithNothing UnivBoolSet

