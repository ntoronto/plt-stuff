{-# LANGUAGE
    ConstraintKinds,
    FlexibleInstances,
    MultiParamTypeClasses,
    TypeFamilies,
    UndecidableInstances,
    RankNTypes,
    StandaloneDeriving #-}

--module Rect where

import GHC.Prim

class Firstable a where
  type Fst a :: *

class Secondable a where
  type Snd a :: *

instance Firstable (a,b) where
  type Fst (a,b) = a

instance Secondable (a,b) where
  type Snd (a,b) = b


class Set s x where
  contains :: s x -> x -> Bool
  singleton :: x -> s x
  empty :: s x
  isEmpty :: s x -> Bool
  meet :: s x -> s x -> s x
  join :: s x -> s x -> s x

data Rect s1 s2 x  =  EmptyRect | Rect (s1 (Fst x)) (s2 (Snd x))

rect :: (Set s1 x1, Set s2 x2) => s1 x1 -> s2 x2 -> Rect s1 s2 (x1,x2)
rect a1 a2 = if (isEmpty a1 || isEmpty a2) then EmptyRect else Rect a1 a2

deriving instance (Show (s1 x1), Show (s2 x2)) => Show (Rect s1 s2 (x1,x2))
deriving instance (Eq (s1 x1), Eq (s2 x2)) => Eq (Rect s1 s2 (x1,x2))

instance Firstable (Rect s1 s2 (x1,x2)) where
  type Fst (Rect s1 s2 (x1,x2)) = x1

instance Secondable (Rect s1 s2 (x1,x2)) where
  type Snd (Rect s1 s2 (x1,x2)) = x2

instance (Set s1 x1, Set s2 x2) => Set (Rect s1 s2) (x1,x2) where
  contains EmptyRect _ = False
  contains (Rect a1 a2) (x1,x2) = contains a1 x1 && contains a2 x2

  singleton (x1,x2) = Rect (singleton x1) (singleton x2)

  empty = EmptyRect

  isEmpty EmptyRect = True
  isEmpty (Rect _ _) = False

  meet EmptyRect _ = EmptyRect
  meet _ EmptyRect = EmptyRect
  meet (Rect a1 a2) (Rect b1 b2) = rect (meet a1 b1) (meet a2 b2)

  join EmptyRect b = b
  join a EmptyRect = a
  join (Rect a1 a2) (Rect b1 b2) = rect (join a1 b1) (join a2 b2)

prod :: (Set s1 x1, Set s2 x2) => s1 x1 -> s2 x2 -> Rect s1 s2 (x1,x2)
prod a b = rect a b

projFst :: (Set s1 x1, Set s2 x2) => Rect s1 s2 (x1,x2) -> s1 x1
projFst EmptyRect = empty
projFst (Rect a1 a2) = a1

projSnd :: (Set s1 x1, Set s2 x2) => Rect s1 s2 (x1,x2) -> s2 x2
projSnd EmptyRect = empty
projSnd (Rect a1 a2) = a2


data Interval x  =  EmptyIvl | Ivl x x  deriving(Show,Eq)

ivl :: Ord x => x -> x -> Interval x
ivl a1 a2  =  if a1 <= a2 then Ivl a1 a2 else EmptyIvl


instance Ord x => Set Interval x where
  contains (Ivl a1 a2) a  =  a1 <= a && a <= a2
  contains EmptyIvl a  =  False

  singleton a  =  Ivl a a

  empty  =  EmptyIvl

  isEmpty EmptyIvl  =  True
  isEmpty (Ivl a1 a2)  =  False

  meet (Ivl a1 a2) (Ivl b1 b2)  =  ivl (max a1 b1) (min a2 b2)
  meet EmptyIvl _  =  EmptyIvl
  meet _ EmptyIvl  =  EmptyIvl

  join (Ivl a1 a2) (Ivl b1 b2)  =  ivl (min a1 b1) (max a2 b2)
  join EmptyIvl a  =  a
  join a EmptyIvl  =  a


main :: IO ()
main = do
  print (ivl 1.0 4.0 :: Interval Float)
  print (meet (ivl 1.0 3.0 :: Interval Float) (ivl 2.0 4.0))
  print (join (ivl 1.0 3.0 :: Interval Float) (ivl 2.0 4.0))
  print (meet (ivl 0.0 1.0 :: Interval Float) (ivl 2.0 3.0))
  print (rect (ivl 1.0 2.0) (ivl 3.0 4.0))
  let a = (rect (ivl 0.0 2.0) (ivl 1 3))
      b = (rect (ivl 1.0 3.0) (ivl 0 2))
    in print ((join a b), (meet a b))
  let a = (rect (ivl 0 2) (ivl 0.0 2.0))
      b = (rect (ivl 1 4) (ivl 3.0 4.0))
    in  print ((join a b), (meet a b))
  print ()

