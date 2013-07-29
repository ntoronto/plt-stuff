import Set
import Rect
import Interval
--import Tree
import PreMapping
import AppxArrow
import PreArrow
import BotArrow
import Control.Arrow

--t = rtreeUnproj [True] (rtreeUnproj [] (ivl MinRLeaf MaxRLeaf) (ivl 0.1 0.9)) (ivl 0.2 0.3)

main :: IO ()
main = do
  --print t
  --print (rtreeProj [True] t)
  --print (rtreeProj [True,False,False,True] t)
  print (ivl 1.0 4.0)

  let i1 = ivl 1.0 3.0
      i2 = ivl 2 4
    in print (meet i1 i2, join i1 i2)

  let i1 = ivl 0 1
      i2 = ivl 2 3
    in print (meet i1 i2, join i1 i2)

  let i1 = ivl 0 1
      i2 = ivl 1 2
    in print (meet i1 i2, join i1 i2)

  let i1 = ivl 0 1
      i2 = ivl 1 2
    in print (prod i1 i2 :: Rect Interval Interval (Integer, Integer))

  let h = appx_fst ~&&& appx_snd :: PreArrow (Rect Interval Interval) (Rect Interval Interval) (Integer,Integer) (Integer,Integer)
      g = runPreArrow h (prod (ivl 0 2) (ivl 0 2))
    in print (preAp g (prod (ivl 0 1) (ivl 0 2)))

  let f = arr fst &&& arr snd :: BotArrow (Integer,Integer) (Integer,Integer)
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

