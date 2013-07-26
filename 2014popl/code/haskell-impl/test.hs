import Set
import Interval
import Tree
import PMappable
import AppxArrow
import PreArrow

t = rtreeUnproj [True] (rtreeUnproj [] (ivl MinRLeaf MaxRLeaf) (ivl 0.1 0.9)) (ivl 0.2 0.3)

main :: IO ()
main = do
  print t
  print (rtreeProj [True] t)
  print (rtreeProj [True,False,False,True] t)
  print (ivl 1.0 4.0 :: Interval Float)
  print (1.0, 4.0)
  print (ivl (1.0, 4.0) (0.0, 2.0) :: Interval (Float,Float))
  print (meet (ivl 1.0 3.0 :: Interval Float) (ivl 2.0 4.0))
  print (join (ivl 1.0 3.0 :: Interval Float) (ivl 2.0 4.0))
  print (meet (ivl 0.0 1.0 :: Interval Float) (ivl 2.0 3.0))
  print (ivl ((1.0,3.0) :: (Float,Float)) (2.0,2.0))
  let a = (ivl (0.0,1) (2.0,3) :: Interval (Float,Integer))
      b = (ivl (1.0,0) (3.0,2))
    in print ((join a b), (meet a b))
  let a = (ivl (0.0,0.0) (2.0,2.0) :: Interval (Float,Float))
      b = (ivl (1.0,3.0) (4.0,4.0))
    in  print ((join a b), (meet a b))
  print ()

