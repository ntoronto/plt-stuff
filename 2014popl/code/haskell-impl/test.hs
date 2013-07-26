import Rect
import Tree
import PMapping
import AppxArrow
import PreArrow

x = rtreeUnproj [True] (rtreeUnproj [] (box MinRLeaf MaxRLeaf) (box 0.1 0.9)) (box 0.2 0.3)

main :: IO ()
main = do
  print x
  print (rtreeProj [True] x)
  print ()

