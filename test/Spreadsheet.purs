module Test.Spreadsheet where

import Prelude
import Data.String (length)
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3)

import Spreadsheet (Cell, Exp, cell, evalExp, get, set)

moduleSuite :: TestSuite
moduleSuite =
  suite "Spreadsheet" do
    test "handles dependency changes" do
      liftEffect changeDependencies >>= Assert.equal (tuple3 3 102 20)
    test "handles dependencies of many types" do
      liftEffect differentTypesDependencies >>= Assert.equal (Tuple 7 5)

-- | Example of a graph of cells.
threeCells :: Exp (Tuple3 (Cell Int) (Cell Int) (Cell Int))
threeCells = do
  a <- cell $ pure 1

  b <- cell $ pure 2

  -- c = a + b
  c <- cell $ do
    aValue <- get a
    bValue <- get b
    pure $ aValue + bValue

  pure $ tuple3 a b c

-- | Example of propagating changes.
changeDependencies :: Effect (Tuple3 Int Int Int)
changeDependencies = do
  Tuple a (Tuple b (Tuple c _)) <- evalExp threeCells

  -- c = a + b = 1 + 2 = 3
  c3 <- evalExp $ get c

  -- a = 100
  -- So c = a + b = 100 + 2 = 102
  set a $ pure 100
  c102 <- evalExp $ get c

  -- a = b*b
  -- b = 4
  -- So c = a + b = 4*4 + 4 = 20
  set a $ do
    bValue <- get b
    pure $ bValue * bValue
  set b $ pure 4
  c20 <- evalExp $ get c

  pure $ tuple3 c3 c102 c20

-- | Example of a graph of cells with different types.
differentTypesCells :: Exp (Tuple3 (Cell String) (Cell Int) (Cell Int))
differentTypesCells = do
  a <- cell $ pure "hello"

  b <- cell $ pure 2

  -- c = a + b
  c <- cell $ do
    aValue <- get a
    bValue <- get b
    pure $ length aValue + bValue

  pure $ tuple3 a b c

-- | Example of propagating changes for cells with different types.
differentTypesDependencies :: Effect (Tuple Int Int)
differentTypesDependencies = do
  Tuple a (Tuple b (Tuple c _)) <- evalExp differentTypesCells

  -- c = length a + b = 5 + 2 = 7
  c7 <- evalExp $ get c

  -- b = 3
  set b $ pure 3

  -- a = "no"
  -- So c = length a + b = 2 + 3 = 5
  set a $ pure "no"
  c5 <- evalExp $ get c

  pure $ Tuple c7 c5
