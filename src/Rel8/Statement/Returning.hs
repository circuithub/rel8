{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}
{-# language TypeApplications #-}

module Rel8.Statement.Returning
  ( Returning
  , numberOfRowsAffected
  , returning

  , ReturningF(..)
  , decodeReturning
  , emptyReturning
  , ppReturning
  )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Monad ( join )
import Data.Foldable ( toList )
import Data.Int ( Int64 )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- free
import Control.Applicative.Free ( Ap(..), runAp_ )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), text )

-- rel8
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Table.Opaleye ( castTable, exprs, view )
import Rel8.Table.Serialize ( Serializable, parse )


type ReturningF :: Type -> Type -> Type
data ReturningF exprs a where
  NumberOfRowsAffected :: ReturningF exprs Int64
  Returning :: Serializable returning a
    => (exprs -> returning)
    -> ReturningF exprs [a]


projections :: Selects names exprs
  => TableSchema names -> ReturningF exprs a -> Maybe (NonEmpty Opaleye.PrimExpr)
projections TableSchema {columns} = \case
  NumberOfRowsAffected -> Nothing
  Returning projection -> Just (exprs (castTable (projection (view columns))))


runReturning :: ()
  => ((Int64 -> a) -> r)
  -> (forall x. Hasql.Row x -> ([x] -> a) -> r)
  -> Ap (ReturningF exprs) a
  -> r
runReturning rowCount rowList = \case
  Pure a -> rowCount (const a)
  Ap NumberOfRowsAffected fs ->
    runReturning
      (rowCount . join)
      (\decoder withRows ->
         rowList decoder (\rows -> withRows rows (length64 rows)))
      fs
  Ap (Returning (_ :: exprs -> returning)) fs ->
    runReturning
      (\withCount ->
         rowList
           decoder'
           (\rows -> withCount (length64 rows) rows))
      (\decoder withRows ->
         rowList
           (liftA2 (,) decoder decoder')
           (\rows -> withRows (fst <$> rows) (snd <$> rows)))
      fs
    where
      decoder' = parse @returning
  where
    length64 :: Foldable f => f x -> Int64
    length64 = fromIntegral . length


decodeReturning :: Ap (ReturningF exprs) a -> Hasql.Result a
decodeReturning =
  runReturning
    (<$> Hasql.rowsAffected)
    (\decoder withRows -> withRows <$> Hasql.rowList decoder)


emptyReturning :: Ap (ReturningF exprs) a -> a
emptyReturning =
  runReturning (\withCount -> withCount 0) (\_ withRows -> withRows [])


-- | 'Rel8.Insert', 'Rel8.Update' and 'Rel8.Delete' all support returning either
-- the number of rows affected, or the actual rows modified.
type Returning :: (Type -> Type -> Type) -> Constraint
class Returning m where
  -- | Return the number of rows affected.
  numberOfRowsAffected :: m exprs Int64

  -- | 'returning' allows you to project out of the affected rows, which can
  -- be useful if you want to log exactly which rows were deleted, or to view
  -- a generated id (for example, if using a column with an autoincrementing
  -- counter via 'Rel8.nextval').
  returning :: Serializable returning a => (exprs -> returning) -> m exprs [a]


ppReturning :: Selects names exprs
  => TableSchema names -> Ap (ReturningF exprs) a -> Doc
ppReturning schema returnings =
  case runAp_ (projections schema) returnings of
    Nothing -> mempty
    Just columns ->
      text "RETURNING" <+> Opaleye.commaV Opaleye.ppSqlExpr (toList sqlExprs)
      where
        sqlExprs = Opaleye.sqlExpr <$> columns
