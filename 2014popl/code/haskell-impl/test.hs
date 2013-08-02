{-# LANGUAGE
    FlexibleInstances,
    FlexibleContexts,
    StandaloneDeriving #-}

import Set
import Rect
import BoolSet
import Interval
import TreeSet
import MaybeSet
import PreMapping
import AppxArrow
import PreArrow
import BotArrow
import Control.Arrow

t = let t0 = unproject [] UnivTreeSet (Ivl 0.1 0.9) :: TreeSet (Interval Float)
        t1 = unproject [True] t0 (Ivl 0.2 0.3)
      in t1

haltOnTrue' :: PreArrow' BoolSet BoolSet
haltOnTrue' = appxIfte' appxId appxId (appxLazy haltOnTrue')

main :: IO ()
main = do
  print t
  print (project [True] t)
  print (project [True,False,False,True] t)
  print (ivl 1.0 4.0)

  let i1 = Ivl 1.0 3.0
      i2 = Ivl 2 4
    in print (meet i1 i2, join i1 i2)

  let i1 = Ivl 0 1
      i2 = Ivl 2 3
    in print (meet i1 i2, join i1 i2)

  let i1 = Ivl 0 1
      i2 = Ivl 1 2
    in print (meet i1 i2, join i1 i2)

  let i1 = Ivl 0 1
      i2 = Ivl 1 2
    in print (prod i1 i2)

  let h = appxFst ~&&& appxSnd :: PreArrow (Rect (Interval Integer) (Interval Integer)) (Rect (Interval Integer) (Interval Integer))
      g = runPreArrow h (prod (Ivl 0 2) (Ivl 0 2))
    in print (preAp g (prod (Ivl 0 1) (Ivl 0 2)))

  let f = arr fst &&& arr snd :: BotArrow (Integer,Integer) (Integer,Integer)
    in print (runBotArrow f (1,1))

  let haltOnTrue = ifte (arr id) (arr id) haltOnTrue :: BotArrow Bool Bool
    in print (runBotArrow haltOnTrue True)

  let haltOnTrue = appxIfte appxId appxId (appxLazy haltOnTrue) :: PreArrow BoolSet BoolSet
    in print (refine haltOnTrue TrueSet TrueSet)

  print (refine' haltOnTrue' UnivBoolSet UnivBoolSet)

  print (refine (runPreArrow' haltOnTrue' j0) 
                (Rect (Rect UnivTreeSet (unproject [] UnivTreeSet (OnlyJust TrueSet)))
                      UnivBoolSet)
                UnivBoolSet)

  print (refine (runPreArrow' haltOnTrue' j0) 
                (Rect (Rect UnivTreeSet (unproject [] UnivTreeSet (OnlyJust FalseSet)))
                      UnivBoolSet)
                UnivBoolSet)

  print ()

