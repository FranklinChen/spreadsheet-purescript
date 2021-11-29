-- | PureScript version of spreadsheet demo from <http://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html Neel Krishnaswami's blog post>.
module Spreadsheet
  ( -- * Types
    Cell
  , Exp
  ,
    -- * Cell operations
    cell
  , get
  , set
  ,
    -- * Expression elimination
    evalExp
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Unsafe (unsafePerformEffect)
import Data.Exists (Exists, mkExists, runExists)

import Data.List (List(..), singleton, unionBy, filter)

-- | For tracking newly created Cell.
newtype Unique = Unique Int

derive instance eqUnique :: Eq Unique

-- | Hold a global counter.
uniqueCounter :: Ref Unique
uniqueCounter = unsafePerformEffect $ Ref.new (Unique 0)

-- | Bump the global counter and return it.
newUnique :: Effect Unique
newUnique = Ref.modify (\(Unique i) -> Unique (i + 1)) uniqueCounter

-- | Container for a value that can depend on other cells
-- through a code expression.
--
-- Has a unique identity, even across different cell types.
newtype Cell a = Cell
  { -- | expression to run
    code :: Ref (Exp a)
  ,
    -- | value memoized from running code
    value :: Ref (Maybe a)
  ,
    -- | cells that were read
    reads :: Ref (List ECell)
  ,
    -- | cells that read this cell
    observers :: Ref (List ECell)
  ,
    -- | globally unique token
    id :: Unique
  }

-- | A computed result, along with the cells read during computation.
data Result a = Result a (List ECell)

-- | An expression that is run to give a result.
newtype Exp a = Exp
  { runExp :: Effect (Result a)
  }

-- | Existential type: "a cell of some type".
-- Used for a heterogeneous list of different types of cells.
type ECell = Exists Cell

-- | Compare cells using ID field.
eqECell :: ECell -> ECell -> Boolean
eqECell = runExists \(Cell x) -> runExists \(Cell y) -> x.id == y.id

instance Applicative Exp where
  pure v = Exp { runExp: pure $ Result v Nil }

instance Bind Exp where
  bind (Exp cmd) f = Exp
    { runExp: do
        Result a cs <- cmd.runExp
        Result b ds <- let Exp newCmd = f a in newCmd.runExp
        pure $ Result b (unionBy eqECell cs ds)
    }

instance Monad Exp

instance Apply Exp where
  apply = ap

instance Functor Exp where
  map = liftM1

-- | Construct a cell.
cell :: forall a. Exp a -> Exp (Cell a)
cell e = Exp
  { runExp: do
      code <- Ref.new e
      value <- Ref.new Nothing
      reads <- Ref.new Nil
      observers <- Ref.new Nil
      id <- newUnique
      let newCell = Cell { code, value, reads, observers, id }
      pure $ Result newCell Nil
  }

-- | Evaluate a cell to get its value.
get :: forall a. Cell a -> Exp a
get theCell@(Cell c) = Exp
  { runExp: do
      cValue <- Ref.read c.value
      case cValue of
        Just v -> pure $ Result v (singleton $ mkExists theCell)
        Nothing -> do
          Result v ds <- do
            Exp exp <- Ref.read c.code
            exp.runExp
          Ref.write (Just v) c.value
          Ref.write ds c.reads
          traverse_
            ( runExists \(Cell d) ->
                Ref.modify_ (Cons $ mkExists theCell) d.observers
            )
            ds
          pure $ Result v (singleton $ mkExists theCell)
  }

-- | Remove a cell from another cell's observers.
removeObserver :: ECell -> ECell -> Effect Unit
removeObserver o = runExists \(Cell c) ->
  Ref.modify_ (filter (not <<< eqECell o)) c.observers

-- | Recursively reset everything in the cell except for its code.
invalidate :: ECell -> Effect Unit
invalidate = runExists \theCell@(Cell c) -> do
  os <- Ref.read c.observers
  rs <- Ref.read c.reads
  Ref.write Nil c.observers
  Ref.write Nothing c.value
  Ref.write Nil c.reads
  traverse_ (removeObserver $ mkExists theCell) rs
  traverse_ invalidate os

-- | Set a cell's code.
set :: forall a. Cell a -> Exp a -> Effect Unit
set theCell@(Cell c) e = do
  Ref.write e c.code
  invalidate $ mkExists theCell

-- | Evaluate an expression.
evalExp :: forall a. Exp a -> Effect a
evalExp (Exp cmd) = do
  Result a _ <- cmd.runExp
  pure a
