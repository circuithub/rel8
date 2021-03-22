{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Statement.Insert
  ( Insert(..)
  , OnConflict(..)
  , insert
  , Column( InsertDefault, InsertExpr )
  , Inserts
  , insertExprs
  ) where

-- base
import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( MonadIO( liftIO ) )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Manipulation as Opaleye
import Rel8.Context ( Column, Context, Defaulting( HasDefault ), Meta( Meta ) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Instances ( Column( ExprColumn, fromExprColumn ) )
import Rel8.Expr.Opaleye ( column, fromPrimExpr )
import Rel8.HTable ( hmap )
import Rel8.Serializable ( Serializable, hasqlRowDecoder )
import Rel8.Statement.Returning ( Returning( Projection, NumberOfRowsAffected ) )
import Rel8.Table ( Table, fromColumns, toColumns )
import Rel8.Table.Congruent ( Congruent, mapTable )
import Rel8.Table.Opaleye ( unpackspec )
import Rel8.Table.Selects ( Selects )
import Rel8.TableSchema ( TableSchema, ddlTable, writer )
import Rel8.TableSchema.ColumnSchema ( ColumnSchema( columnName ), fromColumnSchemaColumn )

-- text
import Data.Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: Type -> Type where
  Insert
    :: Inserts schema row value
    => { into :: TableSchema schema
         -- ^ Which table to insert into.
       , rows :: [row]
         -- ^ The rows to insert.
       , onConflict :: OnConflict
         -- ^ What to do if the inserted rows conflict with data already in the
         -- table.
       , returning :: Returning schema result
         -- ^ What information to return on completion.
       }
    -> Insert result


-- | @OnConflict@ allows you to add an @ON CONFLICT@ clause to an @INSERT@
-- statement.
data OnConflict
  = Abort     -- ^ @ON CONFLICT ABORT@
  | DoNothing -- ^ @ON CONFLICT DO NOTHING@


-- | Run an @INSERT@ statement
--
-- >>> :{
-- insert c Insert
--   { into = authorSchema
--   , rows = [ lit Author{ authorName = "Gabriel Gonzales", authorId = AuthorId 4, authorUrl = Just "https://haskellforall.com" } ]
--   , onConflict = Abort
--   , returning = NumberOfRowsAffected
--   }
-- :}
-- 1
insert :: MonadIO m => Connection -> Insert result -> m result
insert conn Insert{ into, rows, onConflict, returning } = liftIO
  case (rows, returning) of
    ([], NumberOfRowsAffected) ->
      return 0

    ([], Projection _) ->
      return []

    (x:xs, NumberOfRowsAffected) -> Hasql.run session conn >>= either throwIO return where
      session = Hasql.statement () statement where
        statement = Hasql.Statement q Hasql.noParams Hasql.rowsAffected False where
          q = encodeUtf8 $ pack $ Opaleye.arrangeInsertManySql table neRows maybeOnConflict where
            table = ddlTable into (writer into)
            neRows = fmap (fromColumns . hmap insertToExpr . toColumns) $ x :| xs
    (x:xs, Projection p) -> Hasql.run session conn >>= either throwIO return where
      session = Hasql.statement () statement where
        statement = Hasql.Statement q Hasql.noParams (mkDecoder p) False where
          q = encodeUtf8 $ pack $ Opaleye.arrangeInsertManyReturningSql unpackspec table neRows f maybeOnConflict where
            f  = p . mapTable (ExprColumn . column . columnName . fromColumnSchemaColumn)
            table = ddlTable into (writer into)
            neRows = fmap (fromColumns . hmap insertToExpr . toColumns) $ x :| xs

          mkDecoder :: forall row projection a. Serializable projection a => (row -> projection) -> Hasql.Result [a]
          mkDecoder _ = Hasql.rowList (hasqlRowDecoder @projection)
  where
    maybeOnConflict =
      case onConflict of
        DoNothing -> Just Opaleye.DoNothing
        Abort     -> Nothing


instance Context Insert where
  data Column Insert :: Meta -> Type where
    InsertDefault :: Column Insert ('Meta 'HasDefault a)
    InsertExpr :: Expr a -> Column Insert ('Meta defaulting a)


insertToExpr :: Column Insert a -> Column Expr a
insertToExpr = \case
  InsertDefault -> ExprColumn $ fromPrimExpr Opaleye.DefaultInsertExpr
  InsertExpr e  -> ExprColumn e


insertExprs :: Inserts schema inserts exprs => exprs -> inserts
insertExprs = mapTable (InsertExpr . fromExprColumn)


class (Selects schema exprs, Congruent schema inserts, Table Insert inserts, Table ColumnSchema schema) => Inserts schema inserts exprs | schema -> inserts, inserts -> schema
