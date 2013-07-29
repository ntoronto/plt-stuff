{-# LANGUAGE
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    RankNTypes,
    ConstraintKinds,
    MultiParamTypeClasses #-}

module BotArrow where

import GHC.Prim
import Control.Arrow
import Pairable
import AppxArrow
import Control.Monad

type BotArrow x y = Kleisli Maybe x y

pairToEither :: (Bool,x) -> Either x x
pairToEither (a,b) = if a then Left b else Right b

ifte :: ArrowChoice a => a x Bool -> a x y -> a x y -> a x y
ifte f1 f2 f3 = f1 &&& arr id >>> arr pairToEither >>> f2 ||| f3

