module STM.Containers.Private.Focuses
where

import STM.Containers.Private.Prelude hiding (Const)
import Focus.Impure


detalising :: Focus a STM b -> Focus a STM (Decision a b, Maybe a)
detalising =
  \case
    Const decisionFx ->
      Const (fmap (decisionProjection Nothing) decisionFx)
    Lookup decisionFxFn ->
      Lookup (\ lookupResult -> fmap (decisionProjection lookupResult) (decisionFxFn lookupResult))
  where
    decisionProjection lookupResult =
      \ (output, instruction) -> (((output, instruction), lookupResult), instruction)

-- |
-- Projection to a Lookup function.
{-# INLINE lookupFn #-}
lookupFn :: Focus a m b -> (Maybe a -> m (Decision a b))
lookupFn =
  \case
    Const fx ->
      const fx
    Lookup fxFn ->
      fxFn
