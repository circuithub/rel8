{-# language ApplicativeDo #-}
{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language NamedFieldPuns #-}

module Rel8.Table.Window
  ( currentRow
  , lag, lagOn
  , lead, leadOn
  , firstValue, firstValueOn
  , lastValue, lastValueOn
  , nthValue, nthValueOn
  )
where

-- base
import Data.Int (Int32)
import Prelude hiding (null)

-- opaleye
import qualified Opaleye.Window as Opaleye

-- profunctor
import Data.Profunctor (dimap, lmap)

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Null (null, nullify, snull)
import Rel8.Expr.Serialize (litExpr)
import Rel8.Expr.Window
  ( lagExpr, lagExprOn
  , leadExpr, leadExprOn
  , firstValueExpr
  , lastValueExpr
  , nthValueExpr, nthValueExprOn
  )
import Rel8.Schema.HTable (htraverseP)
import Rel8.Schema.HTable.Identity (HIdentity (HIdentity))
import Rel8.Schema.HTable.Label (hlabel)
import Rel8.Schema.HTable.Maybe (HMaybeTable (HMaybeTable))
import Rel8.Schema.HTable.Nullify (hnullify)
import Rel8.Schema.Null (Nullity (NotNull, Null))
import Rel8.Schema.Spec (Spec (..))
import Rel8.Table (Table, fromColumns, toColumns)
import Rel8.Table.Maybe (MaybeTable)
import Rel8.Type.Tag (MaybeTag (IsJust))
import Rel8.Window (Window (Window))


-- | Return every column of the current row of a window query.
currentRow :: Window a a
currentRow = Window $ Opaleye.over (Opaleye.noWindowFunction id) mempty mempty


-- | @'lag' n@ returns the row @n@ rows before the current row in a given
-- window. Returns 'Rel8.nothingTable' if @n@ is out of bounds.
lag :: Table Expr a => Expr Int32 -> Window a (MaybeTable Expr a)
lag n = do
  htag <- lagExprOn n null (\_ -> nullify (litExpr IsJust))
  hjust <- lmap toColumns $ hnullify $ \Spec {info, nullity} ->
    case nullity of
      NotNull -> lagExprOn n (snull info) nullify
      Null -> lagExpr n (snull info)
  pure $ fromColumns $ HMaybeTable (hlabel (HIdentity htag)) (hlabel hjust)


-- | Applies 'lag' to the columns selected by the given function.
lagOn :: Table Expr a => Expr Int32 -> (i -> a) -> Window i (MaybeTable Expr a)
lagOn n f = lmap f (lag n)


-- | @'lead' n@ returns the row @n@ rows after the current row in a given
-- window. Returns 'Rel8.nothingTable' if @n@ is out of bounds.
lead :: Table Expr a => Expr Int32 -> Window a (MaybeTable Expr a)
lead n = do
  htag <- leadExprOn n null (\_ -> nullify (litExpr IsJust))
  hjust <- lmap toColumns $ hnullify $ \Spec {info, nullity} ->
    case nullity of
      NotNull -> leadExprOn n (snull info) nullify
      Null -> leadExpr n (snull info)
  pure $ fromColumns $ HMaybeTable (hlabel (HIdentity htag)) (hlabel hjust)


-- | Applies 'lead' to the columns selected by the given function.
leadOn :: Table Expr a => Expr Int32 -> (i -> a) -> Window i (MaybeTable Expr a)
leadOn n f = lmap f (lead n)


-- | 'firstValue' returns the first row of the window of the current row.
firstValue :: Table Expr a => Window a a
firstValue = dimap toColumns fromColumns $ htraverseP firstValueExpr


-- | Applies 'firstValue' to the columns selected by the given function.
firstValueOn :: Table Expr a => (i -> a) -> Window i a
firstValueOn f = lmap f firstValue


-- | 'lastValue' returns the first row of the window of the current row.
lastValue :: Table Expr a => Window a a
lastValue = dimap toColumns fromColumns $ htraverseP lastValueExpr


-- | Applies 'lastValue' to the columns selected by the given function.
lastValueOn :: Table Expr a => (i -> a) -> Window i a
lastValueOn f = lmap f lastValue


-- | @'nthValue' n@ returns the @n@th row of the window of the current row.
-- Returns 'Rel8.nothingTable' if @n@ is out of bounds.
nthValue :: Table Expr a => Expr Int32 -> Window a (MaybeTable Expr a)
nthValue n = do
  htag <- nthValueExprOn n (\_ -> litExpr IsJust)
  hjust <- lmap toColumns $ hnullify $ \_ -> nthValueExpr n
  pure $ fromColumns $ HMaybeTable (hlabel (HIdentity htag)) (hlabel hjust)


-- | Applies 'nthValue' to the columns selected by the given function.
nthValueOn :: Table Expr a => Expr Int32 -> (i -> a) -> Window i (MaybeTable Expr a)
nthValueOn n f = lmap f (nthValue n)
