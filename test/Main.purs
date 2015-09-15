module Test.Main where

import Prelude
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, AVar(), AVAR())
import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Aff (attempt, later, later', launchAff, Aff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (catchError)
import Control.Plus (empty)
import Data.Either (Either(..), either)
import Data.Functor (($>))
import Data.Maybe (maybe, fromMaybe)
import Data.Array (replicate, head, tail)
import Test.Unit (runTest, test, assertFn)

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

main = runTest do
  test "When the Aff never succeeds" do

    assertFn "The returned Aff should fail" \done ->
      launchAff $ do
        result <- attempt $ reattempt 100 (later empty)
        liftEff $ either (const $ done true) (const $ done false) result

  test "When the timeout will elapse before any attempts to run the Aff are successful" do

    assertFn "The returned Aff should fail" \done ->
      launchAff $ do
        seq <- makeVar' $ failAffsForDurationAndNumberOfAttempts 1000 10
        result <- attempt $ reattempt 100 (affDouble seq)
        liftEff $ either (const $ done true) (const $ done false) result

  test "When the Aff always succeeds" do

    assertFn "The returned Aff be successful" \done ->
      launchAff $ do
        result <- attempt $ reattempt 10000 (later' 1000 $ pure unit)
        liftEff $ either (const $ done false) (const $ done true) result

  test "When the Aff will succeed during an attempt started before the timeout will elapse" do

    assertFn "The returned Aff should be successful" \done ->
      launchAff $ do
        seq <- makeVar' $ failAffsForDurationAndNumberOfAttempts 100 10
        result <- attempt $ reattempt 100000000 (affDouble seq)
        liftEff $ either (const $ done false) (const $ done true) result

    assertFn "The returned Aff should not wait for the timeout to elapse in order to succeed" \done ->
      launchAff $ do
        seq <- makeVar' $ failAffsForDurationAndNumberOfAttempts 100 10
        let parReattempt = Par (reattempt 100000000 (affDouble seq) $> true)
        let parLater = Par (later' 1000 $ pure false)
        result <- runPar $ parReattempt <|> parLater
        liftEff $ done result

  -- TODO: Test that process finishes when attempt is successful.

  -- TODO: Test that the returned Aff only succeeds once.
