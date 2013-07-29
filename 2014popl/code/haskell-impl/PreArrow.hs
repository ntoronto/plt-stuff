{-# LANGUAGE
    ConstraintKinds,
    TypeFamilies,
    MultiParamTypeClasses #-}

module PreArrow where

import Set
import Rect
import PreMapping
import Pairable
import AppxArrow

import GHC.Prim

newtype PreArrow s1 s2 x1 x2  =  PreArrow { runPreArrow :: (s1 x1) -> PreMapping s1 s2 x1 x2 }

instance AppxArrow PreArrow where
  h1 ~>>> h2  =
    PreArrow (\ a -> let h1' = runPreArrow h1 a
                         h2' = runPreArrow h2 (range h1')
                       in preComp h2' h1')

  h1 ~&&& h2  =
    PreArrow (\ a -> prePair (runPreArrow h1 a) (runPreArrow h2 a))

  appx_ifte h1 h2 h3  =
    PreArrow (\ a -> let h1' = runPreArrow h1 a
                         h2' = runPreArrow h2 (preAp h1' (singleton True))
                         h3' = runPreArrow h3 (preAp h1' (singleton False))
                       in prePlus h2' h3')

  appx_id  =
    PreArrow (\ a -> PreMapping a (\ b -> b))

  appx_const y  =
    PreArrow (\ a -> PreMapping (singleton y) (\ b -> if (isEmpty b) then empty else a))

  appx_fst  =
    PreArrow (\ a -> let a1 = projFst a
                         a2 = projSnd a
                       in PreMapping a1 (\ b -> meet a (prod b a2)))

  appx_snd  =
    PreArrow (\ a -> let a1 = projFst a
                         a2 = projSnd a
                       in PreMapping a2 (\ b -> meet a (prod a1 b)))

