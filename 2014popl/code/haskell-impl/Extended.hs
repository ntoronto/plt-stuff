module Extended where

import Rect

data Extended a  =  PosInf | NegInf | Finite a  deriving(Show,Eq)

instance Corner x => Corner (Extended x) where
  lte a PosInf  =  True
  lte NegInf a  =  True
  lte (Finite a1) (Finite a2)  =  lte a1 a2

