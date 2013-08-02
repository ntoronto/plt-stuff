{-# LANGUAGE
    MultiParamTypeClasses #-}

module PreArrow where

import Set
import Rect
import Interval
import MaybeSet
import BoolSet
import TreeSet
import PreMapping
import AppxArrow

newtype PreArrow s1 s2 = PreArrow { runPreArrow :: s1 -> PreMapping s1 s2 }

emptyPreMapping :: (Set s1, Set s2) => PreMapping s1 s2
emptyPreMapping = PreMapping empty (\a -> empty)

instance AppxArrow PreArrow where
  h1 ~>>> h2 =
    PreArrow (\a -> let h1' = runPreArrow h1 a
                        h2' = runPreArrow h2 (preRange h1')
                      in preComp h2' h1')

  h1 ~&&& h2 =
    PreArrow (\a -> prePair (runPreArrow h1 a) (runPreArrow h2 a))

  appxIfte h1 h2 h3 =
    PreArrow (\a -> let h1' = runPreArrow h1 a
                        as2 = preAp h1' (singleton True)
                        as3 = preAp h1' (singleton False)
                      in prePlus (runPreArrow h2 as2) (runPreArrow h3 as3))

  appxLazy h =
    PreArrow (\a -> if a == empty then emptyPreMapping else runPreArrow h a)

  appxId =
    PreArrow (\a -> PreMapping a (\b -> b))

  appxConst y =
    PreArrow (\a -> PreMapping (singleton y) (\b -> a))

  appxFst =
    PreArrow (\a -> let a1 = projFst a
                        a2 = projSnd a
                      in PreMapping a1 (\b -> meet a (prod b a2)))

  appxSnd =
    PreArrow (\a -> let a1 = projFst a
                        a2 = projSnd a
                      in PreMapping a2 (\b -> meet a (prod a1 b)))

refine :: (Set s1, Set s2) => PreArrow s1 s2 -> s1 -> s2 -> s1
refine h as bs = preAp (runPreArrow h as) bs


type PreArrowArgType' s1 = Rect (Rect RSet TSet) s1
newtype PreArrow' s1 s2 =
  PreArrow' { runPreArrow' :: TreeIndex -> PreArrow (PreArrowArgType' s1) s2 }

preArrowLift' :: (Set s1, Set s2) => PreArrow s1 s2 -> PreArrow' s1 s2
preArrowLift' h = PreArrow' (\j -> appxSnd ~>>> h)

instance AppxArrow PreArrow' where
  k1 ~>>> k2 =
    PreArrow' (\j -> appxFst ~&&& runPreArrow' k1 (indexLeft j) ~>>> runPreArrow' k2 (indexRight j))

  k1 ~&&& k2 =
    PreArrow' (\j -> runPreArrow' k1 (indexLeft j) ~&&& runPreArrow' k2 (indexRight j))

  appxIfte k1 k2 k3 =
    PreArrow' (\j -> appxIfte (runPreArrow' k1 (indexLeft j))
                              (runPreArrow' k2 (indexLeft (indexRight j)))
                              (runPreArrow' k3 (indexRight (indexRight j))))

  appxLazy k =
    PreArrow' (\j -> appxLazy (runPreArrow' k j))

  appxId = preArrowLift' appxId
  appxConst y = preArrowLift' (appxConst y)
  appxFst = preArrowLift' appxFst
  appxSnd = preArrowLift' appxSnd

refine' :: (Set s1, Set s2) => PreArrow' s1 s2 -> s1 -> s2 -> PreArrowArgType' s1
refine' k as bs = refine (runPreArrow' k j0) (Rect (Rect UnivTreeSet UnivTreeSet) as) bs


appxRandom :: TreeIndex -> PreArrow RSet (Interval Float)
appxRandom j = PreArrow (\a -> PreMapping (project j a) (\b -> unproject j a b))

appxBranch :: TreeIndex -> PreArrow TSet BoolSet
appxBranch j = PreArrow (\a -> PreMapping (withoutNothing (project j a)) (\b -> unproject j a (OnlyJust b)))

appxRandom' :: Set s1 => PreArrow' s1 (Interval Float)
appxRandom' = PreArrow' (\j -> appxFst ~>>> appxFst ~>>> appxRandom j)

appxBranch' :: Set s1 => PreArrow' s1 BoolSet
appxBranch' = PreArrow' (\j -> appxFst ~>>> appxSnd ~>>> appxBranch j)


appxIfte' :: (Set s1, Set s2)
             => PreArrow' s1 BoolSet -> PreArrow' s1 s2 -> PreArrow' s1 s2 -> PreArrow' s1 s2
appxIfte' k1 k2 k3 = 
  PreArrow'
    (\j ->
      PreArrow
        (\a ->
          let PreMapping ck pk = runPreArrow (runPreArrow' k1 (indexLeft j)) a
              PreMapping cb pb = runPreArrow (runPreArrow' appxBranch' j) a
              c2 = meet (meet ck cb) (singleton True)
              c3 = meet (meet ck cb) (singleton False)
              a2 = meet (pk c2) (pb c2)
              a3 = meet (pk c3) (pb c3)
            in if c2 == empty
               then if c3 == empty
                    then PreMapping empty (\b -> empty)
                    else runPreArrow (runPreArrow' k3 (indexRight (indexRight j))) a2
               else if c3 == empty
                    then runPreArrow (runPreArrow' k2 (indexLeft (indexRight j))) a2
                    else PreMapping universe (\b -> join a2 a3)))
{-
            in if (a2 == empty || a3 == empty)
                  then prePlus (runPreArrow (runPreArrow' k2 (indexLeft (indexRight j))) a2)
                               (runPreArrow (runPreArrow' k3 (indexRight (indexRight j))) a2)
                  else PreMapping universe (\b -> join a2 a3)))
-}

