{-# language FlexibleContexts #-}

module Rel8.Statement.View
  ( createView
  )
where

-- base
import Control.Exception ( throwIO )
import Control.Monad ( (>=>) )
import Data.Foldable ( fold )
import Data.Maybe ( fromMaybe )
import Prelude

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import Rel8.Query ( Query )
import Rel8.Query.SQL ( sqlForQueryWithNames )
import Rel8.Schema.Context ( Name, DB )
import Rel8.Schema.Table ( TableSchema( TableSchema ) )
import Rel8.Table.Alternative ( emptyTable )
import Rel8.Table.Map ( MapTable )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


createView :: MapTable Name DB names exprs
  => TableSchema names -> Query exprs -> Connection -> IO ()
createView (TableSchema name mschema names) query =
  Hasql.run session >=> either throwIO pure
  where
    session = Hasql.statement () statement
    statement = Hasql.Statement bytes params decode prepare
    bytes = encodeUtf8 (Text.pack sql)
    params = Hasql.noParams
    decode = Hasql.noResult
    prepare = False
    sql = "CREATE VIEW " <> title <> " WITH " <> select
      where
        title = case mschema of
          Nothing -> quote name
          Just schema -> quote schema <> "." <> quote name
    select = fromMaybe fallback $ sqlForQueryWithNames names query
      where
        fallback = fold $ sqlForQueryWithNames names emptyTable


quote :: String -> String
quote string = "\"" <> concatMap go string <> "\""
  where
    go '"' = "\"\""
    go c = [c]
