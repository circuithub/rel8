{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Record (
  Record (Record),
  row,
) where

-- base
import Data.Functor.Contravariant ((>$<))
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (fromPrimExpr, toPrimExpr)
import Rel8.Schema.HTable (hfoldMap)
import Rel8.Table (FromExprs, Table, fromResult, toColumns, toResult)
import Rel8.Table.Eq (EqTable)
import Rel8.Table.Ord (OrdTable)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Composite (decodeComposite, encodeComposite)
import Rel8.Type.Eq (DBEq)
import Rel8.Type.Information (TypeInformation (TypeInformation))
import Rel8.Type.Ord (DBOrd)
import qualified Rel8.Type.Information


{-| 'Record' is Rel8's support for PostgreSQL's anonymous record types. Any
'Table' of 'Expr's can be converted to a 'Record' with 'row'.

Note that all of PostgreSQL's limitations on anonymous record types also
apply to @Record@. For example, you won't be able to cast to 'Data.Text.Text'
and back again like you can for other types. This also means that
'Rel8.catListTable' will fail on nested 'Rel8.ListTable's that contain
'Record's.
-}
newtype Record a = Record (FromExprs a)


instance Table Expr a => DBType (Record a) where
  typeInformation =
    TypeInformation
      { decode = Record . fromResult @_ @a <$> decodeComposite
      , encode = toResult @_ @a . (\(Record a) -> a) >$< encodeComposite
      , delimiter = ','
      , typeName = "record"
      }


instance EqTable a => DBEq (Record a)


instance OrdTable a => DBOrd (Record a)


-- | Convert a 'Table' of 'Expr's to a single anonymous record 'Expr'.
row :: Table Expr a => a -> Expr (Record a)
row = fromPrimExpr . Opaleye.FunExpr "ROW" . hfoldMap (pure . toPrimExpr) . toColumns
