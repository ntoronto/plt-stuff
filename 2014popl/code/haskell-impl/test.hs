import Set
import PairSet
import BoolSet
import OrdSet
import TreeSet
import MaybeSet
import PreMapping
import SetArrow
import PreArrow
import PreArrow2
import ValArrow
import BotArrow
import BotArrow2

t = let t0 = unproject [] UnivTreeSet (ivl 0.1 0.9) :: TreeSet (OrdSet Float)
        t1 = unproject [True] t0 (ivl 0.2 0.3)
      in t1

haltOnTrue' :: PreArrow' BoolSet BoolSet
haltOnTrue' = setIfte' setId setId (setLazy haltOnTrue')

main :: IO ()
main = do
  print t
  print (project [True] t)
  print (project [True,False,False,True] t)

  let i1 = ivl 1.0 3.0
      i2 = ivl 2 4
    in print (i1 /\ i2, i1 \/ i2)

  let i1 = ivl 0 1
      i2 = ivl 2 3
    in print (i1 /\ i2, i1 \/ i2)

  let i1 = ivl 0 1
      i2 = ivl 1 2
    in print (i1 /\ i2, i1 \/ i2)

  let i1 = ivl 0 1
      i2 = ivl 1 2
    in print (prod i1 i2)

  let f = valFst -&&& valSnd :: BotArrow (Integer,Integer) (Integer,Integer)
    in print (runBotArrow f (1,1))

  let h = setFst ~&&& setSnd :: PreArrow (PairSet (OrdSet Integer) (OrdSet Integer))
                                         (PairSet (OrdSet Integer) (OrdSet Integer))
      g = runPreArrow h (prod (ivl 0 2) (ivl 0 2))
    in print (preAp g (prod (ivl 0 1) (ivl 0 2)))

  let haltOnTrue = valIfte valId valId haltOnTrue :: BotArrow Bool Bool
    in print (runBotArrow haltOnTrue True)

  let haltOnTrue = setIfte setId setId (setLazy haltOnTrue) :: PreArrow BoolSet BoolSet
    in print (refine haltOnTrue TrueSet TrueSet)

  let haltOnTrue = valIfte' valId valId haltOnTrue :: BotArrow' Bool Bool
    in print (runBotArrow (runBotArrow' haltOnTrue j0)
                          ((AnyTreeVal, (TreeValNode (Just True) AnyTreeVal AnyTreeVal)), False),
              runBotArrow (runBotArrow' haltOnTrue j0)
                          ((AnyTreeVal, (TreeValNode (Just True) AnyTreeVal AnyTreeVal)), True))

  print (refine' haltOnTrue' UnivBoolSet UnivBoolSet)

  print (refine (runPreArrow' haltOnTrue' j0) 
                (prod (prod UnivTreeSet (unproject [] UnivTreeSet (OnlyJust TrueSet)))
                      UnivBoolSet)
                UnivBoolSet)

  print (refine (runPreArrow' haltOnTrue' j0) 
                (prod (prod UnivTreeSet (unproject [] UnivTreeSet (OnlyJust FalseSet)))
                      UnivBoolSet)
                UnivBoolSet)

  print ()

