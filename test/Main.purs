module Test.Main where

import Prelude
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, AVar(), AVAR())
import Control.Monad.Aff (attempt, later, later', Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Parallel.Class (parallel, runParallel)
import Control.Plus (empty)
import Data.Either as Either
import Data.Functor (($>))
import Data.Maybe (maybe, fromMaybe)
import Data.Array (head, tail)
import Data.Unfoldable (replicate)
import Test.Unit (test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Control.Monad.Aff.Reattempt (reattempt)

failAffsForDurationAndNumberOfAttempts :: forall e. Int -> Int -> Array (Aff e Unit)
failAffsForDurationAndNumberOfAttempts timeout attemptCount = seq
  where
  seq = replicate attemptCount (later' interval (empty))
  interval = timeout / attemptCount

affDouble :: forall e. AVar (Array (Aff (avar :: AVAR | e) Unit)) -> Aff (avar :: AVAR | e) Unit
affDouble affsVar = do
  affs <- takeVar affsVar
  putVar affsVar (fromMaybe [] (tail affs))
  maybe (pure unit) id $ head affs

main :: forall eff. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, ref :: REF, avar :: AVAR | eff) Unit
main = runTest do
  test "When the Aff never succeeds the returned Aff should fail" do
    result <- attempt $ reattempt 100 (later empty)
    assert "The returned Aff did not fail" $ Either.isLeft result

  test "When the timeout will elapse before any attempts to run the Aff are successful the returned Aff should fail" do
    seq <- makeVar' $ failAffsForDurationAndNumberOfAttempts 1000 10
    result <- attempt $ reattempt 100 (affDouble seq)
    assert "The returned Aff did not fail" $ Either.isLeft result

  test "When the Aff always succeeds the returned Aff must be successful" do
    result <- attempt $ reattempt 10000 (later' 1000 $ pure unit)
    assert "The returned Aff failed" $ Either.isRight result

  suite "When the Aff will succeed during an attempt started before the timeout will elapse" do

    test "The returned Aff should be successful" do
      seq <- makeVar' $ failAffsForDurationAndNumberOfAttempts 100 10
      result <- attempt $ reattempt 100000000 (affDouble seq)
      assert "The returned Aff failed" $ Either.isRight result

    test "The returned Aff should not wait for the timeout to elapse in order to succeed" do
      seq <- makeVar' $ failAffsForDurationAndNumberOfAttempts 100 10
      let parReattempt = parallel (reattempt 100000000 (affDouble seq) $> true)
      let parLater = parallel (later' 1000 $ pure false)
      result <- runParallel $ parReattempt <|> parLater
      assert "The returned Aff failed" result

  -- TODO: Test that process finishes when attempt is successful.

  -- TODO: Test that the returned Aff only succeeds once.
