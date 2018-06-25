module Test.Main where

import Prelude
import Effect.Aff
import Control.Alt ((<|>))
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (empty)
import Data.Array (head, tail)
import Data.Either as Either
import Data.Int (toNumber)
import Data.Maybe (maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff.AVar (new, take, put, AVar)
import Effect.Aff.Reattempt (reattempt)
import Effect.Console (log)
import Test.Unit (test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

failAffsForDurationAndNumberOfAttempts ∷ Milliseconds → Int → Array (Aff Unit)
failAffsForDurationAndNumberOfAttempts timeout attemptCount = seq
  where
  seq = replicate attemptCount do
    delay interval
    empty
  interval = Milliseconds (unwrap timeout / toNumber attemptCount)

affDouble ∷ AVar (Array (Aff Unit)) -> Aff Unit
affDouble affsVar = do
  affs ← take affsVar
  put (fromMaybe [] (tail affs)) affsVar
  maybe (pure unit) identity $ head affs

main ∷ Effect Unit
main = runTest do
  test "When the Aff never succeeds the returned Aff should fail" do
    result ← attempt $ reattempt (Milliseconds 100.0) do
      delay (Milliseconds 10.0)
      empty
    assert "The returned Aff did not fail" $ Either.isLeft result

  test "When the timeout will elapse before any attempts to run the Aff are successful the returned Aff should fail" do
    seq ← new $ failAffsForDurationAndNumberOfAttempts (Milliseconds 1000.0) 10
    result ← attempt $ reattempt (Milliseconds 100.0) (affDouble seq)
    assert "The returned Aff did not fail" $ Either.isLeft result

  test "When the Aff always succeeds the returned Aff must be successful" do
    result ← attempt $ reattempt (Milliseconds 10000.0) do
      delay (Milliseconds 1000.0)
      pure unit
    assert "The returned Aff failed" $ Either.isRight result

  suite "When the Aff will succeed during an attempt started before the timeout will elapse" do

    test "The returned Aff should be successful" do
      seq ← new $ failAffsForDurationAndNumberOfAttempts (Milliseconds 100.0) 10
      result ← attempt $ reattempt (Milliseconds 100000000.0) (affDouble seq)
      assert "The returned Aff failed" $ Either.isRight result

    test "The returned Aff should not wait for the timeout to elapse in order to succeed" do
      seq ← new $ failAffsForDurationAndNumberOfAttempts (Milliseconds 100.0) 10
      let
        parReattempt = parallel (reattempt (Milliseconds 100000000.0) (affDouble seq) $> true)
        parLater = parallel do
          delay (Milliseconds 1000.0)
          pure false
      result ← sequential $ parReattempt <|> parLater
      assert "The returned Aff failed" result

  -- TODO: Test that process finishes when attempt is successful.

  -- TODO: Test that the returned Aff only succeeds once.
