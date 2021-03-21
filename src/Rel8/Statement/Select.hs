{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Statement.Select
  ( select
  , selectWithNames
  )
where

-- base
import Control.Exception ( throwIO )
import Control.Monad ( (>=>) )
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
import Rel8.Table.Recontextualize ( Selects )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


select :: forall exprs a. Serializable exprs a
  => Query exprs -> Connection -> IO [a]
select query = case sqlForQuery query of
  Nothing -> const $ pure []
  Just sql -> Hasql.run session >=> either throwIO pure
    where
      session = Hasql.statement () statement
      statement = Hasql.Statement bytes params decode prepare
      bytes = encodeUtf8 (Text.pack sql)
      params = Hasql.noParams
      decode = Hasql.rowList (parse @a @exprs)
      prepare = False


selectWithNames :: forall exprs a names.
  ( Selects names exprs
  , Serializable exprs a
  )
  => names -> Query exprs -> Connection -> IO [a]
selectWithNames names query = case sqlForQueryWithNames names query of
  Nothing -> const $ pure []
  Just sql -> Hasql.run session >=> either throwIO pure
    where
      session = Hasql.statement () statement
      statement = Hasql.Statement bytes params decode prepare
      bytes = encodeUtf8 (Text.pack sql)
      params = Hasql.noParams
      decode = Hasql.rowList (parse @a @exprs)
      prepare = False
