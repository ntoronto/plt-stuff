module PMapping where

import Rect

data PMapping x y = PMapping { range :: (Rect y), preimage :: (Rect y -> Rect x) }

pre_ap :: Corner y =>
          PMapping x y -> Rect y -> Rect x
pre_ap (PMapping ys p) bs  =  p (meet bs ys)

pre_pair :: (Corner x, Corner y, Corner z) => PMapping x y -> PMapping x z -> PMapping x (y, z)
pre_pair (PMapping ys py) (PMapping zs pz)  =
  PMapping (prod ys zs)
           (\ (Box (b1, c1) (b2, c2)) -> meet (py (Box b1 b2)) (pz (Box c1 c2)))

pre_comp :: (Corner x, Corner y, Corner z) => PMapping y z -> PMapping x y -> PMapping x z
pre_comp (PMapping zs pz) hy  =
  PMapping zs (\ cs -> pre_ap hy (pz cs))

pre_uplus :: (Corner x, Corner y) => PMapping x y -> PMapping x y -> PMapping x y
pre_uplus h1 h2  =
  PMapping (join (range h1) (range h2))
           (\ bs -> join (pre_ap h1 bs) (pre_ap h2 bs))

