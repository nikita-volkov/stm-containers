module STMContainers.Visit where

import STMContainers.Prelude


-- |
-- A general modification function for some match.
-- By processing a 'Maybe' value it produces some value to emit and 
-- a 'Command' to perform on the match.
-- 
-- The interpretation of this function is up to the context APIs.
type Visit a r = Maybe a -> (r, Command a)

-- |
-- A monadic version of 'Visit'.
type VisitM m a r = Maybe a -> m (r, Command a)

-- |
-- What to do with the visited value.
-- 
-- The interpretation of the commands is up to the context APIs.
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


-- * Constructors for common pure patterns
-------------------------

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:adjust adjust>@.
adjust :: (a -> a) -> Visit a ()
adjust f = maybe ((), Keep) (\a -> ((), Replace (f a)))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:update update>@.
update :: (a -> Maybe a) -> Visit a ()
update f = maybe ((), Keep) (\a -> ((), maybe Remove Replace (f a)))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:alter alter>@.
alter :: (Maybe a -> Maybe a) -> Visit a ()
alter f = ((),) . maybe Remove Replace . f

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:insert insert>@.
insert :: a -> Visit a ()
insert a = const ((), Replace a)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:delete delete>@.
delete :: Visit a ()
delete = const ((), Remove)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:lookup lookup>@.
lookup :: Visit a (Maybe a)
lookup r = (r, Keep)


-- * Constructors for monadic patterns
-------------------------

-- |
-- A monadic version of 'adjust'.
adjustM :: (Monad m) => (a -> m a) -> VisitM m a ()
adjustM f = maybe (return ((), Keep)) (liftM (((),) . Replace) . f)

-- |
-- A monadic version of 'update'.
updateM :: (Monad m) => (a -> m (Maybe a)) -> VisitM m a ()
updateM f = maybe (return ((), Keep)) (liftM (((),) . maybe Remove Replace) . f)

-- |
-- A monadic version of 'alter'.
alterM :: (Monad m) => (Maybe a -> m (Maybe a)) -> VisitM m a ()
alterM f = liftM (((),) . maybe Remove Replace) . f
