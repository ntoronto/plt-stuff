module AppxArrow where

import Rect

infixr 1 >>>
infixr 3 &&&

class AppxArrow a where
  (>>>) :: (Corner x, Corner y, Corner z) => a x y -> a y z -> a x z
  (&&&) :: (Corner x, Corner y, Corner z) => a x y -> a x z -> a x (y,z)
  ifte :: (Corner x, Corner y) => a x Bool -> a x y -> a x y -> a x y
  id :: Corner x => a x x
  const :: Corner y => y -> a x y
  fst :: (Corner x, Corner y) => a (x,y) x
  snd :: (Corner x, Corner y) => a (x,y) y

