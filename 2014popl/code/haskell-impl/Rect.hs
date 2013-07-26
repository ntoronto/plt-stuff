--{-# LANGUAGE #-}

module Rect where

class Corner x where
  lte :: x -> x -> Bool
  cmin :: x -> x -> x
  cmax :: x -> x -> x
  cmin a1 a2  =  if lte a1 a2 then a1 else a2
  cmax a1 a2  =  if lte a1 a2 then a2 else a1

instance Corner Float where
  lte a1 a2  =  a1 <= a2

instance Corner Integer where
  lte a1 a2  =  a1 <= a2

instance Corner Bool where
  lte a1 a2  =  a1 <= a2

instance (Corner x, Corner y) => Corner (x,y) where
  lte (a1,b1) (a2,b2)  =  lte a1 a2 && lte b1 b2


data Extended a  =  PosInf | NegInf | Finite a  deriving(Show,Eq)

instance Corner x => Corner (Extended x) where
  lte a PosInf  =  True
  lte NegInf a  =  True
  lte (Finite a1) (Finite a2)  =  lte a1 a2


data Rect x  =  Box x x | Empty  deriving(Show,Eq)

box :: Corner x => x -> x -> Rect x
box a1 a2  =  if lte a1 a2 then Box a1 a2 else Empty

meet :: Corner x => Rect x -> Rect x -> Rect x
meet (Box a1 a2) (Box b1 b2)  =  box (cmax a1 b1) (cmin a2 b2)
meet Empty _  =  Empty
meet _ Empty  =  Empty

join :: Corner x => Rect x -> Rect x -> Rect x
join (Box a1 a2) (Box b1 b2)  =  box (cmin a1 b1) (cmax a2 b2)
join Empty a  =  a
join a Empty  =  a

contains :: Corner x => Rect x -> x -> Bool
contains (Box a1 a2) a  =  lte a1 a && lte a a2
contains Empty a  =  False

prod :: (Corner x, Corner y) => Rect x -> Rect y -> Rect (x,y)
prod (Box a1 a2) (Box b1 b2)  =  box (a1,b1) (a2,b2)
prod Empty _  =  Empty
prod _ Empty  =  Empty

rect_fst :: Corner x => Rect (x,y) -> Rect x
rect_fst (Box (a1,b1) (a2,b2))  =  box a1 a2
rect_fst Empty  =  Empty

rect_snd :: Corner y => Rect (x,y) -> Rect y
rect_snd (Box (a1,b1) (a2,b2))  =  box b1 b2
rect_snd Empty  =  Empty

singleton :: x -> Rect x
singleton a  =  Box a a

{-|
main :: IO ()
main = do
  print (Box 1.0 4.0)
  print (1.0, 4.0)
  print (Box (1.0, 4.0) (0.0, 2.0))
  print (meet (Box 1.0 3.0 :: Rect Float) (Box 2.0 4.0))
  print (join (Box 1.0 3.0 :: Rect Float) (Box 2.0 4.0))
  print (meet (Box 0.0 1.0 :: Rect Float) (Box 2.0 3.0))
  print (box ((1.0,3.0) :: (Float,Float)) (2.0,2.0))
  let a = (Box (0.0,1) (2.0,3) :: Rect (Float,Integer))
      b = (Box (1.0,0) (3.0,2))
    in print ((join a b), (meet a b))
  let a = (Box (0.0,0.0) (2.0,2.0) :: Rect (Float,Float))
      b = (Box (1.0,3.0) (4.0,4.0))
    in  print ((join a b), (meet a b))
  print ()
|-}

