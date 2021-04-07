{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Statement.Select
  ( select
  , selectWithNames
  )
where

-- base
import Control.Exception ( throwIO )
import Prelude

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import Rel8.Query ( Query )
import Rel8.Query.SQL ( sqlForQuery, sqlForQueryWithNames )
import Rel8.Schema.Name ( Selects )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | Run a @SELECT@ query, returning all rows.
select :: forall exprs a. Serializable exprs a
  => Connection -> Query exprs -> IO [a]
select c query = case sqlForQuery query of
  Nothing -> pure []
  Just sql -> Hasql.run session c >>= either throwIO pure
    where
      session = Hasql.statement () statement
      statement = Hasql.Statement bytes params decode prepare
      bytes = encodeUtf8 (Text.pack sql)
      params = Hasql.noParams
      decode = Hasql.rowList (parse @exprs @a)
      prepare = False


selectWithNames :: forall exprs a names.
  ( Selects names exprs
  , Serializable exprs a
  )
  => Connection -> names -> Query exprs -> IO [a]
selectWithNames c names query = case sqlForQueryWithNames names query of
  Nothing -> pure []
  Just sql -> Hasql.run session c >>= either throwIO pure
    where
      session = Hasql.statement () statement
      statement = Hasql.Statement bytes params decode prepare
      bytes = encodeUtf8 (Text.pack sql)
      params = Hasql.noParams
      decode = Hasql.rowList (parse @exprs @a)
      prepare = False
