{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Statement.View
  ( createView
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
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Insert ( ppInto )
import Rel8.Statement.Select ( ppSelect )

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), text )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | Given a 'TableSchema' and 'Query', @createView@ runs a @CREATE VIEW@
-- statement that will save the given query as a view. This can be useful if
-- you want to share Rel8 queries with other applications.
createView :: Selects names exprs
  => Connection -> TableSchema names -> Query exprs -> IO ()
createView connection schema query =
  Hasql.run session connection >>= either throwIO pure
  where
    session = Hasql.statement () statement
    statement = Hasql.Statement bytes params decode prepare
    bytes = encodeUtf8 (Text.pack sql)
    params = Hasql.noParams
    decode = Hasql.noResult
    prepare = False
    sql = show doc
    doc = ppCreateView schema query


ppCreateView :: Selects names exprs
  => TableSchema names -> Query exprs -> Doc
ppCreateView schema query =
  text "CREATE VIEW" <+>
  ppInto schema $$
  text "AS" <+>
  ppSelect query
