{-# LANGUAGE
    FlexibleInstances,
    FlexibleContexts,
    StandaloneDeriving #-}

import Set
import Rect
import Interval
import Tree
import PreMapping
import AppxArrow
import PreArrow
import BotArrow
import Control.Arrow

t = do
  t <- rsetUnproj [] RFull (Ivl 0.1 0.9)
  t <- rsetUnproj [True] t (Ivl 0.2 0.3)
  return t

--deriving instance (Set Interval x, Show (Interval x)) => Show (Maybe (Interval x))
--deriving instance (Set s1 x1, Set s2 x2, Eq (s1 x1), Eq (s2 x2)) => Eq (Rect s1 s2 (x1,x2))


main :: IO ()
main = do
  print t
  --print (rtreeProj [True] t)
  --print (rtreeProj [True,False,False,True] t)
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

  let h = appxFst ~&&& appxSnd :: PreArrow (Rect Interval Interval) (Rect Interval Interval) (Integer,Integer) (Integer,Integer)
      g = runPreArrow h (prod (Ivl 0 2) (Ivl 0 2))
    in print (preAp g (prod (Ivl 0 1) (Ivl 0 2)))

  let f = arr fst &&& arr snd :: BotArrow (Integer,Integer) (Float,Float)
    in print (runBotArrow f (1,1))

  let haltOnTrue = ifte (arr id) (arr id) haltOnTrue :: BotArrow Bool Bool
    in print (runBotArrow haltOnTrue True)
{-|
  let a = (prod (ivl 0.0 2.0) (ivl 1 3))
      b = (prod (ivl 1.0 3.0) (ivl 0 2))
    in print ((join a b), (meet a b))
  let a = (prod (ivl 0 2) (ivl 0.0 2.0))
      b = (prod (ivl 1 4) (ivl 3.0 4.0))
    in  print ((join a b), (meet a b))
|-}
  print ()

