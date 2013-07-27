{-# LANGUAGE
    MultiParamTypeClasses #-}

module PreArrow where

import Set
import Rect
import PreMapping
import Pairable
import AppxArrow

import GHC.Prim

newtype PreArrow x1 x2  =  PreArrow { prearrow :: x1 -> PreMapping x1 x2 }

instance AppxArrow PreArrow Set Rect where
  h1 >>> h2  =
    PreArrow (\ a -> let h1' = prearrow h1 a
                         h2' = prearrow h2 (range h1')
                       in preComp h2' h1')

  h1 &&& h2  =
    PreArrow (\ a -> prePair (prearrow h1 a) (prearrow h2 a))

  ifte h1 h2 h3  =
    PreArrow (\ a -> let h1' = prearrow h1 a
                         h2' = prearrow h2 (preAp h1' (singleton True))
                         h3' = prearrow h3 (preAp h1' (singleton False))
                       in prePlus h2' h3')

  id  =
    PreArrow (\ a -> PreMapping a (\ b -> b))

  const y  =
    PreArrow (\ a -> PreMapping (singleton y) (\ b -> if (isEmpty b) then empty else a))

  fst  =
    PreArrow (\ a -> let a1 = projFst a
                         a2 = projSnd a
                       in PreMapping a1 (\ b -> meet a (prod b a2)))

  snd  =
    PreArrow (\ a -> let a1 = projFst a
                         a2 = projSnd a
                       in PreMapping a2 (\ b -> meet a (prod a1 b)))

