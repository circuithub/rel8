module Rel8.Statement.Using (
  ppFrom,
  ppUsing,
)
where

-- base
import Prelude

-- pretty
import Text.PrettyPrint (Doc, parens, text, (<+>))

-- rel8
import Rel8.Query (Query)
import Rel8.Schema.Table (TableSchema (..), ppTable)
import Rel8.Statement.Select (Optimized (..), ppPrimSelect)


ppFrom :: Query a -> Maybe (Doc, a)
ppFrom = ppJoin "FROM"


ppUsing :: Query a -> Maybe (Doc, a)
ppUsing = ppJoin "USING"


ppJoin :: String -> Query a -> Maybe (Doc, a)
ppJoin clause join = do
  doc <- case ofrom of
    Empty -> Nothing
    Unit -> Just mempty
    Optimized doc -> Just $ text clause <+> parens doc <+> ppTable alias
  pure (doc, a)
  where
    alias = TableSchema{name = "T1", schema = Nothing, columns = ()}
    (ofrom, a) = ppPrimSelect join
