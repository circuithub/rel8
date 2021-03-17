{-# language BlockArguments #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Insert ( Insert(..), OnConflict(..), insert ) where

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

-- opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Manipulation as Opaleye

-- rel8
import Rel8.Expr ( column )
import Rel8.Query ( hasqlRowDecoder, unpackspec )
import Rel8.Returning ( Returning( Projection, NumberOfRowsAffected ) )
import Rel8.Serializable ( Serializable )
import Rel8.Table.Congruent ( mapTable )
import Rel8.Table.Selects ( Selects )
import Rel8.TableSchema ( TableSchema, ddlTable, writer )
import Rel8.TableSchema.ColumnSchema ( ColumnSchema( columnName ) )

-- text
import Data.Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: Type -> Type where
  Insert
    :: Selects schema value
    => { into :: TableSchema schema
         -- ^ Which table to insert into.
       , rows :: [value]
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
            neRows = x :| xs

    (x:xs, Projection p) -> Hasql.run session conn >>= either throwIO return where
      session = Hasql.statement () statement where
        statement = Hasql.Statement q Hasql.noParams (mkDecoder p) False where
          q = encodeUtf8 $ pack $ Opaleye.arrangeInsertManyReturningSql unpackspec table neRows f maybeOnConflict where
            f  = p . mapTable (column . columnName)
            table = ddlTable into (writer into)
            neRows = x :| xs

          mkDecoder :: forall row projection a. Serializable projection a => (row -> projection) -> Hasql.Result [a]
          mkDecoder _ = Hasql.rowList (hasqlRowDecoder @projection)
  where
    maybeOnConflict =
      case onConflict of
        DoNothing -> Just Opaleye.DoNothing
        Abort     -> Nothing
