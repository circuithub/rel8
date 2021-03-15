{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Opaleye
  ( aggregator
  , binaryspec
  , distinctspec
  , tableFields
  , unpackspec
  , valuesspec
  , view
  , writer
  )
where

-- base
import Data.Functor ( (<&>), void )
import Prelude hiding ( undefined )

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Opaleye.Internal.Table as Opaleye

-- rel8
import Rel8.Expr.Aggregate ( Aggregate( Aggregate ), Aggregator( Aggregator ) )
import qualified Rel8.Expr.Aggregate
import Rel8.Expr.Opaleye ( traversePrimExpr, fromPrimExpr, toPrimExpr )
import Rel8.Schema.Context ( Aggregation(..), DB(..), Insert(..), Name(..) )
import Rel8.Schema.HTable ( htabulateA, htabulate, hfield, htraverse, hspecs )
import Rel8.Schema.Recontextualize ( Recontextualize )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Undefined ( undefined )


aggregator :: Recontextualize Aggregation DB aggregates exprs
  => Opaleye.Aggregator aggregates exprs
aggregator = Opaleye.Aggregator $ Opaleye.PackMap $ \f ->
  fmap fromColumns .
  htraverse (\(Aggregation aggregate) -> case aggregate of
    Aggregate {aggregator = maggregator, input, output} ->
      let
        aggregator' = maggregator <&>
          \Aggregator {operation, ordering, distinction} ->
            (operation, ordering, distinction)
      in
        DB . output <$> f (aggregator', input)) .
  toColumns


binaryspec :: (Table a, Context a ~ DB) => Opaleye.Binaryspec a a
binaryspec = Opaleye.Binaryspec $ Opaleye.PackMap $ \f (as, bs) ->
  fmap fromColumns $ htabulateA $ \field ->
    case (hfield (toColumns as) field, hfield (toColumns bs) field) of
      (DB a, DB b) -> DB . fromPrimExpr <$> f (toPrimExpr a, toPrimExpr b)


distinctspec :: (Table a, Context a ~ DB) => Opaleye.Distinctspec a a
distinctspec =
  Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap $ \f ->
    fmap fromColumns .
    htraverse (\(DB a) -> DB . fromPrimExpr <$> f (Nothing, toPrimExpr a)) .
    toColumns


tableFields ::
  ( Recontextualize Name DB names exprs
  , Recontextualize Name Insert names inserts
  )
  => names -> Opaleye.TableFields inserts exprs
tableFields = Opaleye.TableFields <$> writer <*> view


unpackspec :: (Table a, Context a ~ DB) => Opaleye.Unpackspec a a
unpackspec = Opaleye.Unpackspec $ Opaleye.PackMap $ \f ->
  fmap fromColumns .
  htraverse (\(DB a) -> DB <$> traversePrimExpr f a) .
  toColumns


valuesspec :: (Table a, Context a ~ DB) => Opaleye.ValuesspecSafe a a
valuesspec = Opaleye.ValuesspecSafe (toPackMap undefined) unpackspec


view :: Recontextualize Name DB names exprs => names -> Opaleye.View exprs
view (toColumns -> names) = Opaleye.View $ fromColumns $
  htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> case hfield names field of
      Name name -> DB (fromPrimExpr (Opaleye.BaseTableAttrExpr name))


writer :: forall names inserts exprs. Recontextualize Name Insert names inserts
  => names -> Opaleye.Writer inserts exprs
writer (toColumns -> names) =
  Opaleye.Writer $ Opaleye.PackMap $ \f (fmap toColumns -> as) ->
    void $ htabulateA @(Columns names) $ \field -> case hfield names field of
      n@(Name name) -> n <$
        f (fmap (insertToExpr . (`hfield` field)) as, name)
  where
    insertToExpr :: Insert spec -> Opaleye.PrimExpr
    insertToExpr = \case
      RequiredInsert a -> toPrimExpr a
      OptionalInsert ma -> maybe Opaleye.DefaultInsertExpr toPrimExpr ma


toPackMap :: (Table a, Context a ~ DB)
  => a -> Opaleye.PackMap Opaleye.PrimExpr Opaleye.PrimExpr () a
toPackMap as = Opaleye.PackMap $ \f () ->
  fmap fromColumns $
  htraverse (\(DB a) -> DB <$> traversePrimExpr f a) $
  toColumns as
