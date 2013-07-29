module Extended where

data Extended a  =  PosInf | NegInf | Finite a  deriving(Show,Eq)

instance Ord x => Ord (Extended x) where
  a <= PosInf  =  True
  NegInf <= a  =  True
  Finite a1 <= Finite a2  =  a1 <= a2

