{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Table.Opaleye ( unpackspec, binaryspec, distinctspec, valuesspec ) where

-- rel8
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import Rel8.Context ( Context )
import Rel8.DatabaseType ( DatabaseType( DatabaseType, typeName ) )
import Rel8.Expr ( Expr, fromPrimExpr, toPrimExpr, traversePrimExpr, unsafeCastExpr )
import Rel8.HTable ( HTable( HField, hfield, htraverse, htabulate, hdbtype ) )
import Rel8.Info ( Info( Null, NotNull ) )
import Rel8.Table ( Columns, Table( toColumns, fromColumns ) )
import Rel8.Table.Congruent ( traverseTable, zipTablesWithM )


unpackspec :: Table Expr row => Opaleye.Unpackspec row row
unpackspec =
  Opaleye.Unpackspec $ Opaleye.PackMap \f ->
    fmap fromColumns . htraverse (traversePrimExpr f) . addCasts . toColumns
  where
    addCasts :: forall f. HTable f => f (Context Expr) -> f (Context Expr)
    addCasts columns = htabulate go
      where
        go :: forall x. HField f x -> Expr x
        go i = case hfield hdbtype i of
          NotNull t -> unsafeCastExpr (typeName t) (hfield columns i)
          Null t -> unsafeCastExpr (typeName t) (hfield columns i)


binaryspec :: Table Expr a => Opaleye.Binaryspec a a
binaryspec =
  Opaleye.Binaryspec $ Opaleye.PackMap \f (a, b) ->
    zipTablesWithM (\x y -> fromPrimExpr <$> f (toPrimExpr x, toPrimExpr y)) a b


distinctspec :: Table Expr a => Opaleye.Distinctspec a a
distinctspec =
  Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap \f ->
    traverseTable (\x -> fromPrimExpr <$> f (Nothing, toPrimExpr x))


valuesspec :: forall expr. Table Expr expr => Opaleye.Valuesspec expr expr
valuesspec = Opaleye.ValuesspecSafe packmap unpackspec
  where
    packmap :: Opaleye.PackMap Opaleye.PrimExpr Opaleye.PrimExpr () expr
    packmap = Opaleye.PackMap \f () ->
      fmap fromColumns $
        htraverse (traversePrimExpr f) $
          htabulate @(Columns expr) @Expr \i ->
            case hfield (hdbtype @(Columns expr)) i of
              NotNull databaseType -> fromPrimExpr $ nullPrimExpr databaseType
              Null databaseType -> fromPrimExpr $ nullPrimExpr databaseType
        where
          nullPrimExpr :: DatabaseType a -> Opaleye.PrimExpr
          nullPrimExpr DatabaseType{ typeName } =
            Opaleye.CastExpr typeName (Opaleye.ConstExpr Opaleye.NullLit)
