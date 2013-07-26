module Tree where

import Rect


type TreeIndex  =  [Bool]

indexLeft :: TreeIndex -> TreeIndex
indexLeft j = True : j

indexRight :: TreeIndex -> TreeIndex
indexRight j = False : j


data RTree  =  MinRLeaf | MaxRLeaf | RNode !Float !RTree !RTree
  deriving(Show,Eq)

rtreeRandom :: RTree -> Float
rtreeRandom MinRLeaf  =  0.0
rtreeRandom MaxRLeaf  =  1.0
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

rtreeProj :: TreeIndex -> Rect RTree -> Rect Float
rtreeProj j Empty  =  Empty
rtreeProj [] (Box t1 t2)  =  Box (rtreeRandom t1) (rtreeRandom t2)
rtreeProj  (True:j) (Box t1 t2)  =  rtreeProj j (Box (rtreeLeft  t1) (rtreeLeft  t2))
rtreeProj (False:j) (Box t1 t2)  =  rtreeProj j (Box (rtreeRight t1) (rtreeRight t2))

rtreeUnproj :: TreeIndex -> Rect RTree -> Rect Float -> Rect RTree
rtreeUnproj j Empty _  =  Empty
rtreeUnproj j _ Empty  =  Empty

rtreeUnproj [] (Box t1 t2) (Box y1 y2)  =
  let RNode x1 l1 r1 = rtreeExpand t1
      RNode x2 l2 r2 = rtreeExpand t2
    in case meet (Box x1 x2) (Box y1 y2) of
         Box z1 z2 -> Box (rnode z1 l1 r1) (rnode z2 l2 r2)
         Empty -> Empty

rtreeUnproj  (True:j) (Box t1 t2) (Box y1 y2)  =
  let RNode x1 l1 r1 = rtreeExpand t1
      RNode x2 l2 r2 = rtreeExpand t2
    in case rtreeUnproj j (Box l1 l2) (Box y1 y2) of
         Box l1 l2 -> Box (rnode x1 l1 r1) (rnode x2 l2 r2)
         Empty -> Empty

rtreeUnproj (False:j) (Box t1 t2) (Box y1 y2)  =
  let RNode x1 l1 r1 = rtreeExpand t1
      RNode x2 l2 r2 = rtreeExpand t2
    in case rtreeUnproj j (Box r1 r2) (Box y1 y2) of
         Box r1 r2 -> Box (rnode x1 l1 r1) (rnode x2 l2 r2)
         Empty -> Empty

