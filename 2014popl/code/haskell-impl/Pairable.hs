{-# LANGUAGE TypeFamilies #-}

module Pairable where

class Pairable t where
  type Fst t :: *
  type Snd t :: *
  pairFst :: t -> Fst t
  pairSnd :: t -> Snd t

instance Pairable (x,y) where
  type Fst (x,y) = x
  type Snd (x,y) = y
  pairFst (a,b) = a
  pairSnd (a,b) = b

