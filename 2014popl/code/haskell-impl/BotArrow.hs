module BotArrow where

import Control.Arrow

type BotArrow x y = Kleisli Maybe x y

runBotArrow :: BotArrow x y -> x -> Maybe y
runBotArrow = runKleisli

pairToEither :: (Bool,x) -> Either x x
pairToEither (a,b) = if a then Left b else Right b

ifte :: ArrowChoice a => a x Bool -> a x y -> a x y -> a x y
ifte f1 f2 f3 = f1 &&& arr id >>> arr pairToEither >>> f2 ||| f3

const :: Arrow a => y -> a x y
const b = arr (\ a -> b)

