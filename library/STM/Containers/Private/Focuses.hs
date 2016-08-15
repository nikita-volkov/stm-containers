module STM.Containers.Private.Focuses
where

import STM.Containers.Private.Prelude hiding (Const)
import Focus.Impure


detalising :: Focus a STM b -> Focus a STM (b, Instruction a, Maybe a)
detalising focus =
  \ lookupResult -> fmap (decisionProjection lookupResult) (focus lookupResult)
  where
    decisionProjection lookupResult (output, instruction) =
      ((output, instruction, lookupResult), instruction)

{-# INLINE mapInput #-}
mapInput :: Functor m => (a2 -> a1) -> (a1 -> a2) -> Focus a1 m b -> Focus a2 m b
mapInput proj1 proj2 focus =
  fmap (second (fmap proj2)) . focus . fmap proj1

{-# INLINE mapOutput #-}
mapOutput :: Functor m => (b1 -> b2) -> Focus a m b1 -> Focus a m b2
mapOutput proj =
  (fmap . fmap) (first proj)
