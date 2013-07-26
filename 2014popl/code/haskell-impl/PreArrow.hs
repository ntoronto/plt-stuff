{-# LANGUAGE ConstraintKinds, TypeFamilies, UndecidableInstances #-}

module PreArrow where

import Set
import PMappable
import AppxArrow

newtype PreArrow s x y  =  PreArrow { prearrow :: s x -> PMapping s x y }

instance PMappable s => AppxArrow (PreArrow s) where
  type AppxArrowCtxt x = (PMappableCtxt x, SetCtxt x)

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
    PreArrow (\ a -> PMapping a (\ b -> b))

  const y  =
    PreArrow (\ a -> PMapping (singleton y) (\ b -> if (isEmpty b) then empty else a))

  fst  =
    PreArrow (\ a -> let a1 = projFst a
                         a2 = projSnd a
                       in PMapping a1 (\ b -> meet a (prod b a2)))

  snd  =
    PreArrow (\ a -> let a1 = projFst a
                         a2 = projSnd a
                       in PMapping a2 (\ b -> meet a (prod a1 b)))

{-|
import Box
import Tree

newtype PreArrow' s x y  =  PreArrow' { prearrow' :: TreeIndex -> PreArrow s (RTree,x) y }

transPreArrow' :: (Corner x, Corner y) => PreArrow s x y -> PreArrow' s x y
transPreArrow' f  =  PreArrow' (\ j -> AppxArrow.snd >>> f)

instance Set s => AppxArrow (PreArrow' s) where
  k1 >>> k2  =
    PreArrow' (\ j -> let h1 = prearrow' k1 (indexLeft  j)
                          h2 = prearrow' k2 (indexRight j)
                        in AppxArrow.fst &&& h1 >>> h2)

  k1 &&& k2  =
    PreArrow' (\ j -> let h1 = prearrow' k1 (indexLeft  j)
                          h2 = prearrow' k2 (indexRight j)
                        in h1 &&& h2)

  ifte k1 k2 k3  =
    PreArrow' (\ j -> let h1 = prearrow' k1 (indexLeft j)
                          h2 = prearrow' k2 (indexLeft  (indexLeft j))
                          h3 = prearrow' k3 (indexRight (indexLeft j))
                        in ifte h1 h2 h3)
|-}

