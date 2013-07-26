{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances, TypeSynonymInstances #-}

module Set where

import GHC.Prim

class Pairable p where
  pair :: x -> y -> p x y
  pairFst :: p x y -> x
  pairSnd :: p x y -> y

instance Pairable (,) where
  pair a b  =  (a,b)
  pairFst (a,b)  =  a
  pairSnd (a,b)  =  b
  

class Set s where
  type SetCtxt v :: Constraint
  empty :: SetCtxt v => s v
  isEmpty :: SetCtxt v => s v -> Bool
  contains :: SetCtxt v => s v -> v -> Bool
  singleton :: SetCtxt v => v -> s v
  meet :: SetCtxt v => s v -> s v -> s v
  join :: SetCtxt v => s v -> s v -> s v
  prod :: (SetCtxt v1, SetCtxt v2, Pairable p) => s v1 -> s v2 -> s (p v1 v2)
  projFst :: (SetCtxt v1, SetCtxt v2, Pairable p) => s (p v1 v2) -> s v1
  projSnd :: (SetCtxt v1, SetCtxt v2, Pairable p) => s (p v1 v2) -> s v2

