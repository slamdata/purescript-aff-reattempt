module Control.Monad.Aff.Reattempt where

import Prelude

import Effect.Aff (Aff, delay, forkAff, supervise)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Time.Duration (Milliseconds)

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
reattempt ∷ ∀ a. Milliseconds → Aff a → Aff a
reattempt ms aff = supervise do
  elapsed ← liftEffect $ new false
  _ ← forkAff do
    delay ms
    liftEffect $ write true elapsed
  let attempt = aff `catchError` \error → do
        shouldRethrow ← liftEffect $ read elapsed
        if shouldRethrow
          then throwError error
          else attempt
  attempt
