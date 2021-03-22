{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Statement.Select ( selectQuery, showQuery, select ) where

-- base
import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( MonadIO( liftIO ) )
import Data.Foldable ( fold )

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.Print as Opaleye ( formatAndShowSQL )
import qualified Opaleye.Internal.QueryArr as Opaleye
import Rel8.Expr ( Expr )
import Rel8.Query ( Query( Query ) )
import Rel8.Serializable ( Serializable, hasqlRowDecoder )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( unpackspec )

-- text
import Data.Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | Run a @SELECT@ query, returning all rows.
select :: forall row haskell m. (Serializable row haskell, MonadIO m) => Connection -> Query row -> m [haskell]
select conn query = liftIO case selectQuery query of
  Nothing -> return []
  Just neQuery ->
    Hasql.run session conn >>= either throwIO return
    where
      session = Hasql.statement () statement
      statement = Hasql.Statement q params (Hasql.rowList (hasqlRowDecoder @row)) prepare
      q = encodeUtf8 (pack neQuery)
      params = Hasql.noParams
      prepare = False


selectQuery :: forall a . Table Expr a => Query a -> Maybe String
selectQuery (Query opaleye) = showSqlForPostgresExplicit
  where
    showSqlForPostgresExplicit =
      case Opaleye.runQueryArrUnpack unpackspec opaleye of
        (x, y, z) -> Opaleye.formatAndShowSQL True (x , Opaleye.optimize y , z)


-- | Convert a query to a 'String' containing the query as a @SELECT@
-- statement.
showQuery :: Table Expr a => Query a -> String
showQuery = fold . selectQuery
