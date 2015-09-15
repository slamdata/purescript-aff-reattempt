module Control.Monad.Aff.Reattempt where

import Prelude

import Control.Monad.Aff (Aff(), forkAff, later', cancel)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef, REF())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Eff.Exception (Error(), error)

-- | Repeatedly runs the specified asynchronous computation until either it succeeds or
-- | the specified timeout elapses. A running continuation won't be cancelled.
reattempt :: forall e a. Int -> Aff (ref :: REF | e) a -> Aff (ref :: REF | e) a
reattempt ms aff = do
  elapsed <- liftEff $ newRef false
  forkedTimeout <- forkAff (later' ms $ liftEff $ writeRef elapsed true)
  let attempt = aff `catchError` \error -> do
        shouldRethrow <- liftEff $ readRef elapsed
        if shouldRethrow
          then throwError (error :: Error)
          else attempt
  result <- attempt
  -- Process continues after returned aff succeeds if forked timeout isn't cancelled
  cancel forkedTimeout (error "")
  pure result
