{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Rel8.Statement.Returning (
  Returning (NumberOfRowsAffected, Projection),
  decodeReturning,
  ppReturning,
)
where

-- base
import Control.Applicative (liftA2)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint (Doc, text, (<+>))

-- rel8
import Rel8.Schema.Name (Selects)
import Rel8.Schema.Table (TableSchema (..))
import Rel8.Table.Opaleye (castTable, exprs, view)
import Rel8.Table.Serialize (Serializable, parse)

-- semigroupoids
import Data.Functor.Apply (Apply, (<.>))


{- | 'Rel8.Insert', 'Rel8.Update' and 'Rel8.Delete' all support returning either
the number of rows affected, or the actual rows modified.
-}
type Returning :: Type -> Type -> Type
data Returning names a where
  Pure :: a -> Returning names a
  Ap :: Returning names (a -> b) -> Returning names a -> Returning names b
  -- | Return the number of rows affected.
  NumberOfRowsAffected :: Returning names Int64
  -- | 'Projection' allows you to project out of the affected rows, which can
  -- be useful if you want to log exactly which rows were deleted, or to view
  -- a generated id (for example, if using a column with an autoincrementing
  -- counter via 'Rel8.nextval').
  Projection ::
    (Selects names exprs, Serializable returning a) =>
    (exprs -> returning) ->
    Returning names [a]


instance Functor (Returning names) where
  fmap f = \case
    Pure a -> Pure (f a)
    Ap g a -> Ap (fmap (f .) g) a
    m -> Ap (Pure f) m


instance Apply (Returning names) where
  (<.>) = Ap


instance Applicative (Returning names) where
  pure = Pure
  (<*>) = Ap


projections ::
  () =>
  TableSchema names ->
  Returning names a ->
  Maybe (NonEmpty Opaleye.PrimExpr)
projections schema@TableSchema{columns} = \case
  Pure _ -> Nothing
  Ap f a -> projections schema f <> projections schema a
  NumberOfRowsAffected -> Nothing
  Projection f -> Just (exprs (castTable (f (view columns))))


runReturning ::
  () =>
  ((Int64 -> a) -> r) ->
  (forall x. Hasql.Row x -> ([x] -> a) -> r) ->
  Returning names a ->
  r
runReturning rowCount rowList = \case
  Pure a -> rowCount (const a)
  Ap fs as ->
    runReturning
      ( \withCount ->
          runReturning
            (\withCount' -> rowCount (withCount <*> withCount'))
            (\decoder -> rowList decoder . liftA2 withCount length64)
            as
      )
      ( \decoder withRows ->
          runReturning
            (\withCount -> rowList decoder $ withRows <*> withCount . length64)
            ( \decoder' withRows' ->
                rowList (liftA2 (,) decoder decoder') $
                  withRows <$> fmap fst <*> withRows' . fmap snd
            )
            as
      )
      fs
  NumberOfRowsAffected -> rowCount id
  Projection (_ :: exprs -> returning) -> rowList decoder' id
    where
      decoder' = parse @returning
  where
    length64 :: Foldable f => f x -> Int64
    length64 = fromIntegral . length


decodeReturning :: Returning names a -> Hasql.Result a
decodeReturning =
  runReturning
    (<$> Hasql.rowsAffected)
    (\decoder withRows -> withRows <$> Hasql.rowList decoder)


ppReturning :: TableSchema names -> Returning names a -> Doc
ppReturning schema returning = case projections schema returning of
  Nothing -> mempty
  Just columns ->
    text "RETURNING" <+> Opaleye.commaV Opaleye.ppSqlExpr (toList sqlExprs)
    where
      sqlExprs = Opaleye.sqlExpr <$> columns
