module Control.Monad.Aff.Reattempt where

import Prelude

import Control.Monad.Aff (Aff(), forkAff, later', cancel)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef, REF())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Eff.Exception (Error(), error)

-- | `reattempt` repeatedly attempts to run the provided `Aff` until either an attempt
-- | succeeds or the provided timeout elapses.
-- |
-- | After the timeout elapses no more attempts will be made but the last attempt will
-- | not be cancelled. Each attempt either fails or succeeds. The timeout has no effect on
-- | the outcome of an attempt.
-- |
-- | When an attempt to run the provided `Aff` succeeds the `Aff` returned by `reattempt`
-- | will succeed. When no attempts succeed the `Aff` returned by `reattempt` will fail
-- | with the `Error` raised by the last attempt.
reattempt ∷ ∀ e a. Int → Aff (ref ∷ REF | e) a → Aff (ref ∷ REF | e) a
reattempt ms aff = do
  elapsed ← liftEff $ newRef false
  forkedTimeout ← forkAff (later' ms $ liftEff $ writeRef elapsed true)
  let attempt = aff `catchError` \error → do
        shouldRethrow ← liftEff $ readRef elapsed
        if shouldRethrow
          then throwError (error ∷ Error)
          else attempt
  result ← attempt
  -- Process continues after returned aff succeeds if forked timeout isn't cancelled
  cancel forkedTimeout (error "")
  pure result
