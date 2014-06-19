module STMContainers.Alter where

import STMContainers.Prelude


-- |
-- A general modification function for some match.
type Alter a r = Maybe a -> (r, Command a)

-- |
-- A monadic version of 'Alter'.
type AlterM m a r = Maybe a -> m (r, Command a)

-- |
-- An alteration command.
data Command a =
  Keep |
  Remove |
  Replace a
  deriving (Functor)

-- |
-- Convert a pure 'Alter' into a monadic one.
monadize :: (Monad m) => Alter a r -> AlterM m a r
monadize = fmap return


-- * Lifters for standard altering patterns
-------------------------

liftUpdate :: (a -> a) -> Alter a ()
liftUpdate f = \case
  Nothing -> ((), Keep)
  Just a -> ((), Replace (f a))

liftUpdateM :: (Monad m) => (a -> m a) -> AlterM m a ()
liftUpdateM f = \case
  Nothing -> return ((), Keep)
  Just a -> f a >>= return . ((), ) . Replace
