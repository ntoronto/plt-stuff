{-# LANGUAGE
    FlexibleInstances #-}

module PreArrow2 where

import Set
import Rect
import Interval
import MaybeSet
import BoolSet
import TreeSet
import PreMapping
import SetArrow
import PreArrow

-- Instances of infinite vector sets: for unit intervals and boolean+bottom sets.

type RSet = TreeSet (Interval Float)
type TSet = TreeSet (MaybeSet BoolSet)

instance TreeAxisSet (Interval Float) where
  fullAxis = ivl 0.0 1.0

instance TreeAxisSet (MaybeSet BoolSet) where
  fullAxis = WithNothing UnivBoolSet

-- Another preimage arrow, which can be used as a translation target for probabilistic, partial
-- programs. See Toronto & McCarthy 2014.

type PreArrowArgType' s1 = Rect (Rect RSet TSet) s1
newtype PreArrow' s1 s2 =
  PreArrow' { runPreArrow' :: TreeIndex -> PreArrow (PreArrowArgType' s1) s2 }

preArrowLift' :: (Set s1, Set s2) => PreArrow s1 s2 -> PreArrow' s1 s2
preArrowLift' h = PreArrow' (\j -> setSnd ~>>> h)

instance SetArrow PreArrow' where
  k1 ~>>> k2 =
    PreArrow' (\j -> setFst ~&&& runPreArrow' k1 (indexLeft j) ~>>> runPreArrow' k2 (indexRight j))

  k1 ~&&& k2 =
    PreArrow' (\j -> runPreArrow' k1 (indexLeft j) ~&&& runPreArrow' k2 (indexRight j))

  setIfte k1 k2 k3 =
    PreArrow' (\j -> setIfte (runPreArrow' k1 (indexLeft j))
                              (runPreArrow' k2 (indexLeft (indexRight j)))
                              (runPreArrow' k3 (indexRight (indexRight j))))

  setLazy k =
    PreArrow' (\j -> setLazy (runPreArrow' k j))

  setId = preArrowLift' setId
  setConst y = preArrowLift' (setConst y)
  setFst = preArrowLift' setFst
  setSnd = preArrowLift' setSnd

refine' :: (Set s1, Set s2) => PreArrow' s1 s2 -> s1 -> s2 -> PreArrowArgType' s1
refine' k as bs = refine (runPreArrow' k j0) (Rect UnivRect as) bs

-- For getting the random number at the expression index:
setRandom :: TreeIndex -> PreArrow RSet (Interval Float)
setRandom j = PreArrow (\a -> PreMapping (project j a) (\b -> unproject j a b))

setRandom' :: Set s1 => PreArrow' s1 (Interval Float)
setRandom' = PreArrow' (\j -> setFst ~>>> setFst ~>>> setRandom j)


-- For indexing branch traces (not used directly in translations):
setBranch :: TreeIndex -> PreArrow TSet BoolSet
setBranch j = PreArrow (\a -> PreMapping (withoutNothing (project j a)) (\b -> unproject j a (OnlyJust b)))

setBranch' :: Set s1 => PreArrow' s1 BoolSet
setBranch' = PreArrow' (\j -> setFst ~>>> setSnd ~>>> setBranch j)

-- A "lazier" setIfte, which never takes more than one branch

setIfte' :: (Set s1, Set s2)
             => PreArrow' s1 BoolSet -> PreArrow' s1 s2 -> PreArrow' s1 s2 -> PreArrow' s1 s2
setIfte' k1 k2 k3 = 
  PreArrow'
    (\j ->
      PreArrow
        (\a ->
          let PreMapping ck pk = runPreArrow (runPreArrow' k1 (indexLeft j)) a
              PreMapping cb pb = runPreArrow (runPreArrow' setBranch' j) a
              c2 = ck /\ cb /\ singleton True
              c3 = ck /\ cb /\ singleton False
              a2 = pk c2 /\ pb c2
              a3 = pk c3 /\ pb c3
            in if c2 == empty
               then if c3 == empty
                    then emptyPreMapping
                    else runPreArrow (runPreArrow' k3 (indexRight (indexRight j))) a2
               else if c3 == empty
                    then runPreArrow (runPreArrow' k2 (indexLeft (indexRight j))) a2
                    else PreMapping universe (\b -> a2 \/ a3)))
{-
            in if (a2 == empty || a3 == empty)
                  then prePlus (runPreArrow (runPreArrow' k2 (indexLeft (indexRight j))) a2)
                               (runPreArrow (runPreArrow' k3 (indexRight (indexRight j))) a2)
                  else PreMapping universe (\b -> join a2 a3)))
-}

