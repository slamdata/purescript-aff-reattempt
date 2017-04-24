module Test.Main where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (attempt, delay, Aff)
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, AVar, AVAR)
import Control.Monad.Aff.Reattempt (reattempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Parallel.Class (parallel)
import Control.Plus (empty)
import Data.Array (head, tail)
import Data.Either as Either
import Data.Int (toNumber)
import Data.Maybe (maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable (replicate)
import Test.Unit (test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

failAffsForDurationAndNumberOfAttempts ∷ ∀ e. Milliseconds → Int → Array (Aff e Unit)
failAffsForDurationAndNumberOfAttempts timeout attemptCount = seq
  where
  seq = replicate attemptCount do
    delay interval
    empty
  interval = Milliseconds (unwrap timeout / toNumber attemptCount)

affDouble ∷ ∀ e. AVar (Array (Aff (avar ∷ AVAR | e) Unit)) → Aff (avar ∷ AVAR | e) Unit
affDouble affsVar = do
  affs ← takeVar affsVar
  putVar affsVar (fromMaybe [] (tail affs))
  maybe (pure unit) id $ head affs

main ∷ ∀ eff. Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, ref ∷ REF, avar ∷ AVAR | eff) Unit
main = runTest do
  test "When the Aff never succeeds the returned Aff should fail" do
    result ← attempt $ reattempt (Milliseconds 100.0) do
      delay (Milliseconds 10.0)
      empty
    assert "The returned Aff did not fail" $ Either.isLeft result

  test "When the timeout will elapse before any attempts to run the Aff are successful the returned Aff should fail" do
    seq ← makeVar' $ failAffsForDurationAndNumberOfAttempts (Milliseconds 1000.0) 10
    result ← attempt $ reattempt (Milliseconds 100.0) (affDouble seq)
    assert "The returned Aff did not fail" $ Either.isLeft result

  test "When the Aff always succeeds the returned Aff must be successful" do
    result ← attempt $ reattempt (Milliseconds 10000.0) do
      delay (Milliseconds 1000.0)
      pure unit
    assert "The returned Aff failed" $ Either.isRight result

  suite "When the Aff will succeed during an attempt started before the timeout will elapse" do

    test "The returned Aff should be successful" do
      seq ← makeVar' $ failAffsForDurationAndNumberOfAttempts (Milliseconds 100.0) 10
      result ← attempt $ reattempt (Milliseconds 100000000.0) (affDouble seq)
      assert "The returned Aff failed" $ Either.isRight result

    test "The returned Aff should not wait for the timeout to elapse in order to succeed" do
      seq ← makeVar' $ failAffsForDurationAndNumberOfAttempts (Milliseconds 100.0) 10
      let
        parReattempt = parallel (reattempt (Milliseconds 100000000.0) (affDouble seq) $> true)
        parLater = parallel do
          delay (Milliseconds 1000.0)
          pure false
      result ← unwrap $ parReattempt <|> parLater
      assert "The returned Aff failed" result

  -- TODO: Test that process finishes when attempt is successful.

  -- TODO: Test that the returned Aff only succeeds once.
