module STMContainers.Alter where

import STMContainers.Prelude

-- |
-- A general modification function for some match.
type Alter a r = Maybe a -> (r, Command a)

-- |
-- A monadic version of 'Alter'.
type AlterM m a r = Maybe a -> m (r, Command a)

-- |
-- What to do to.
data Command a =
  Keep |
  Remove |
  Replace a
