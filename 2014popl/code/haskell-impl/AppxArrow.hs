{-# LANGUAGE
    ConstraintKinds,
    MultiParamTypeClasses #-}

module AppxArrow where

import GHC.Prim
import Pairable

infixr 1 >>>
infixr 3 &&&

class AppxArrow a ctxt prod where
  (>>>) :: (ctxt s1 x, ctxt s2 y, ctxt s3 z)
           => a (s1 x) (s2 y) -> a (s2 y) (s3 z) -> a (s1 x) (s3 z)

  (&&&) :: (Pairable y, ctxt s1 x, ctxt s2 (Fst y), ctxt s3 (Snd y))
           => a (s1 x) (s2 (Fst y)) -> a (s1 x) (s3 (Snd y)) -> a (s1 x) (prod s2 s3 y)

  ifte :: (ctxt s1 x, ctxt s2 Bool, ctxt s3 y)
          => a (s1 x) (s2 Bool) -> a (s1 x) (s3 y) -> a (s1 x) (s3 y) -> a (s1 x) (s3 y)

  id :: ctxt s1 x => a (s1 x) (s1 x)
  const :: (ctxt s1 x, ctxt s2 y) => y -> a (s1 x) (s2 y)
  fst :: (Pairable x, ctxt s1 (Fst x), ctxt s2 (Snd x)) => a (prod s1 s2 x) (s1 (Fst x))
  snd :: (Pairable x, ctxt s1 (Fst x), ctxt s2 (Snd x)) => a (prod s1 s2 x) (s2 (Snd x))

