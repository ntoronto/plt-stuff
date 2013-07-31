{-# LANGUAGE
    MultiParamTypeClasses #-}

module PreArrow where

import Set
import Rect
--import Tree
import PreMapping
import AppxArrow

newtype PreArrow s1 s2 x1 x2 = PreArrow { runPreArrow :: s1 x1 -> Maybe (PreMapping s1 s2 x1 x2) }

instance AppxArrow PreArrow where
  h1 ~>>> h2 =
    PreArrow (\ a -> do h1' <- runPreArrow h1 a
                        let h2' = runPreArrow h2 (range h1')
                          in preComp h2' (Just h1'))

  h1 ~&&& h2 =
    PreArrow (\ a -> prePair (runPreArrow h1 a) (runPreArrow h2 a))

  -- This is WRONG: if one preimage set is empty, it should carry on with the other
  appxIfte h1 h2 h3  =
    PreArrow (\ a -> let h1' = runPreArrow h1 a
                       in do x2 <- preAp h1' (singleton True)
                             x3 <- preAp h1' (singleton False)
                             prePlus (runPreArrow h2 x2) (runPreArrow h3 x3))

  appxId =
    PreArrow (\ a -> Just (PreMapping a (\ b -> Just b)))

  appxConst y =
    PreArrow (\ a -> Just (PreMapping (singleton y) (\ b -> Just a)))

  appxFst =
    PreArrow (\ a -> let a1 = projFst a
                         a2 = projSnd a
                       in Just (PreMapping a1 (\ b -> meet a (prod b a2))))

  appxSnd =
    PreArrow (\ a -> let a1 = projFst a
                         a2 = projSnd a
                       in Just (PreMapping a2 (\ b -> meet a (prod a1 b))))

