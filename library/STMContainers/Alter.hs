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
{-# INLINE monadize #-}
monadize :: (Monad m) => Alter a r -> AlterM m a r
monadize = fmap return


-- * Constructors for standard alteration patterns
-------------------------

update :: (a -> a) -> Alter a ()
update f = maybe ((), Keep) (\a -> ((), Replace (f a)))

insert :: a -> Alter a ()
insert a = const ((), Replace a)

delete :: Alter a ()
delete = const ((), Remove)

lookup :: Alter a (Maybe a)
lookup r = (r, Keep)


-- * Constructors for monadic alteration patterns
-------------------------

updateM :: (Monad m) => (a -> m a) -> AlterM m a ()
updateM f = \case
  Nothing -> return ((), Keep)
  Just a -> f a >>= return . ((), ) . Replace
