{-# LANGUAGE
    MultiParamTypeClasses #-}

module Tree where

import Set
import Interval
--import System.Random


type TreeIndex = [Bool]

indexLeft :: TreeIndex -> TreeIndex
indexLeft j = True : j

indexRight :: TreeIndex -> TreeIndex
indexRight j = False : j





data RSet x = RFull | RSetNode !(Interval Float) !(RSet x) !(RSet x)
  deriving(Show,Eq)

data RVal = RAny | RValNode !Float !RVal !RVal
  deriving(Show,Eq)

instance Set RSet RVal where
  meet RFull a = Just a
  meet a RFull = Just a
  meet (RSetNode xs1 l1 r1) (RSetNode xs2 l2 r2) =
    do xs <- meet xs1 xs2
       l <- meet l1 l2
       r <- meet r1 r2
       return (RSetNode xs l r)

  join RFull _ = RFull
  join _ RFull = RFull
  join (RSetNode xs1 ls1 rs1) (RSetNode xs2 ls2 rs2) =
    RSetNode (Set.join xs1 xs2) (Set.join ls1 ls2) (Set.join rs1 rs2)

  contains RFull _ = True
  contains (RSetNode xs ls rs) (RValNode x l r) =
    contains xs x && contains ls l && contains rs r
  contains (RSetNode xs ls rs) RAny = False

  singleton RAny = RFull
  singleton (RValNode x l r) = RSetNode (singleton x) (singleton l) (singleton r)

type RSource = RSet RVal

rsetProj :: TreeIndex -> RSource -> Interval Float
rsetProj j RFull = unitIvl
rsetProj [] (RSetNode xs _ _) = xs
rsetProj  (True:j) (RSetNode _ l _) = rsetProj j l
rsetProj (False:j) (RSetNode _ _ r) = rsetProj j r

rsetUnproj :: TreeIndex -> RSource -> Interval Float -> Maybe RSource

rsetUnproj j RFull ys = rsetUnproj j (RSetNode unitIvl RFull RFull) ys

rsetUnproj [] (RSetNode xs ls rs) ys =
  do zs <- meet xs ys
     return (RSetNode zs ls rs)

rsetUnproj  (True:j) (RSetNode xs ls rs) ys =
  do ls' <- rsetUnproj j ls ys
     return (RSetNode xs ls' rs)

rsetUnproj (False:j) (RSetNode xs ls rs) ys =
  do rs' <- rsetUnproj j rs ys
     return (RSetNode xs ls rs')

{-|
rsetSample :: RSource -> IO RVal
rsetSample RFull = return RAny
rsetSample (RSetNode (Interval x1 x2) ls rs) =
  do x <- random
     l <- rsetSample ls
     r <- rsetSample rs
     return (RValNode (x1 + x * (x2 - x1)) l r)
|-}

