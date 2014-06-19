module STMContainers.Visit where

import STMContainers.Prelude


-- |
-- A general modification function for some match.
type Visit a r = Maybe a -> (r, Command a)

-- |
-- A monadic version of 'Visit'.
type VisitM m a r = Maybe a -> m (r, Command a)

-- |
-- What to do with the visited value.
data Command a =
  Keep |
  Remove |
  Replace a
  deriving (Functor)

-- |
-- Convert a pure 'Visit' into a monadic one.
{-# INLINE monadize #-}
monadize :: (Monad m) => Visit a r -> VisitM m a r
monadize = fmap return


-- * Constructors for standard patterns
-------------------------

update :: (a -> a) -> Visit a ()
update f = maybe ((), Keep) (\a -> ((), Replace (f a)))

insert :: a -> Visit a ()
insert a = const ((), Replace a)

delete :: Visit a ()
delete = const ((), Remove)

lookup :: Visit a (Maybe a)
lookup r = (r, Keep)


-- * Constructors for monadic patterns
-------------------------

updateM :: (Monad m) => (a -> m a) -> VisitM m a ()
updateM f = \case
  Nothing -> return ((), Keep)
  Just a -> f a >>= return . ((), ) . Replace
