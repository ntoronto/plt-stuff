{-# LANGUAGE
    TypeFamilies,
    MultiParamTypeClasses #-}

module BotArrow where

import GHC.Prim
import Pairable
import AppxArrow
import Control.Monad


newtype BotArrow x y  =  BotArrow { botarrow :: x -> Maybe y }

newtype Pair v1 v2 x = Pair { pair :: (v1 (Fst x), v2 (Snd x)) }

instance Pairable x => Pairable (Pair v1 v2 x) where
  type Fst (Pair v1 v2 x) = Fst x
  type Snd (Pair v1 v2 x) = Snd x
  --pairFst (Pair a) = pairFst a
  --pairSnd (Pair a) = pairSnd a

class Id s x where
  take :: s x -> x

instance AppxArrow BotArrow Id Pair where

  f1 >>> f2  =
    BotArrow (\ x -> do y <- botarrow f1 x
                        botarrow f2 y)

  f1 &&& f2  =
    BotArrow (\ x -> do y1 <- botarrow f1 x
                        y2 <- botarrow f2 x
                        return (Pair (y1,y2)))

  ifte f1 f2 f3  =
    BotArrow (\ x -> case botarrow f1 x of
                       Nothing -> Nothing
                       Just b -> if BotArrow.take b then botarrow f2 x else botarrow f3 x)
{-|

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
|-}


