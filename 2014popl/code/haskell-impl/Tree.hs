module Tree where

import Set
import Interval


type TreeIndex  =  [Bool]

indexLeft :: TreeIndex -> TreeIndex
indexLeft j = True : j

indexRight :: TreeIndex -> TreeIndex
indexRight j = False : j


data RTree  =  REmpty | RFull | RNode !Float !RTree !RTree
  deriving(Show,Eq)

rtreeRandom :: RTree -> Float
rtreeRandom RFull  =  0.0
rtreeRandom (RNode x _ _)  =  x

rtreeLeft :: RTree -> RTree
rtreeLeft MinRLeaf  =  MinRLeaf
rtreeLeft MaxRLeaf  =  MaxRLeaf
rtreeLeft (RNode _ l _)  =  l

rtreeRight :: RTree -> RTree
rtreeRight MinRLeaf  =  MinRLeaf
rtreeRight MaxRLeaf  =  MaxRLeaf
rtreeRight (RNode _ _ r)  =  r

rnode :: Float -> RTree -> RTree -> RTree
rnode 0.0 MinRLeaf MinRLeaf  =  MinRLeaf
rnode 1.0 MaxRLeaf MaxRLeaf  =  MaxRLeaf
rnode x l r  =  RNode x l r

rtreeExpand :: RTree -> RTree
rtreeExpand MinRLeaf  =  RNode 0.0 MinRLeaf MinRLeaf
rtreeExpand MaxRLeaf  =  RNode 1.0 MaxRLeaf MaxRLeaf
rtreeExpand t  =  t

instance Corner RTree where
  lte MinRLeaf MinRLeaf  =  True
  lte MinRLeaf MaxRLeaf  =  True
  lte MaxRLeaf MaxRLeaf  =  True
  lte MaxRLeaf MinRLeaf  =  False
  lte (RNode x1 l1 r1) (RNode x2 l2 r2)  =  x1 <= x2 && lte l1 l2 && lte r1 r2
  lte t1 t2  =  lte (rtreeExpand t1) (rtreeExpand t2)

rtreeProj :: TreeIndex -> Interval RTree -> Interval Float
rtreeProj j Empty  =  Empty
rtreeProj [] (Ivl t1 t2)  =  Ivl (rtreeRandom t1) (rtreeRandom t2)
rtreeProj  (True:j) (Ivl t1 t2)  =  rtreeProj j (Ivl (rtreeLeft  t1) (rtreeLeft  t2))
rtreeProj (False:j) (Ivl t1 t2)  =  rtreeProj j (Ivl (rtreeRight t1) (rtreeRight t2))

rtreeUnproj :: TreeIndex -> Interval RTree -> Interval Float -> Interval RTree
rtreeUnproj j Empty _  =  Empty
rtreeUnproj j _ Empty  =  Empty

rtreeUnproj [] (Ivl t1 t2) (Ivl y1 y2)  =
  let RNode x1 l1 r1 = rtreeExpand t1
      RNode x2 l2 r2 = rtreeExpand t2
    in case meet (Ivl x1 x2) (Ivl y1 y2) of
         Ivl z1 z2 -> Ivl (rnode z1 l1 r1) (rnode z2 l2 r2)
         Empty -> Empty

rtreeUnproj  (True:j) (Ivl t1 t2) (Ivl y1 y2)  =
  let RNode x1 l1 r1 = rtreeExpand t1
      RNode x2 l2 r2 = rtreeExpand t2
    in case rtreeUnproj j (Ivl l1 l2) (Ivl y1 y2) of
         Ivl l1 l2 -> Ivl (rnode x1 l1 r1) (rnode x2 l2 r2)
         Empty -> Empty

rtreeUnproj (False:j) (Ivl t1 t2) (Ivl y1 y2)  =
  let RNode x1 l1 r1 = rtreeExpand t1
      RNode x2 l2 r2 = rtreeExpand t2
    in case rtreeUnproj j (Ivl r1 r2) (Ivl y1 y2) of
         Ivl r1 r2 -> Ivl (rnode x1 l1 r1) (rnode x2 l2 r2)
         Empty -> Empty

