module PreArrow where

import Rect
import Tree
import PMapping
import AppxArrow

newtype PreArrow x y  =  PreArrow { prearrow :: Rect x -> PMapping x y }

instance AppxArrow PreArrow where
  h1 >>> h2  =
    PreArrow (\ a -> let h1' = prearrow h1 a
                         h2' = prearrow h2 (range h1')
                       in pre_comp h2' h1')

  h1 &&& h2  =
    PreArrow (\ a -> pre_pair (prearrow h1 a) (prearrow h2 a))

  ifte h1 h2 h3  =
    PreArrow (\ a -> let h1' = prearrow h1 a
                         h2' = prearrow h2 (pre_ap h1' (singleton True))
                         h3' = prearrow h3 (pre_ap h1' (singleton False))
                       in pre_uplus h2' h3')

  id  =
    PreArrow (\ a -> PMapping a (\ b -> b))

  const y  =
    PreArrow (\ a -> PMapping (singleton y) (let p Empty = Empty; p _ = a in p))

  fst  =
    PreArrow (\ a -> let a1 = rect_fst a
                         a2 = rect_snd a
                       in PMapping a1 (\ b -> meet a (prod b a2)))

  snd  =
    PreArrow (\ a -> let a1 = rect_fst a
                         a2 = rect_snd a
                       in PMapping a2 (\ b -> meet a (prod a1 b)))


newtype PreArrow' x y  =  PreArrow' { prearrow' :: TreeIndex -> PreArrow (RTree,x) y }

transPreArrow' :: (Corner x, Corner y) => PreArrow x y -> PreArrow' x y
transPreArrow' f  =  PreArrow' (\ j -> AppxArrow.snd >>> f)

instance AppxArrow PreArrow' where
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

