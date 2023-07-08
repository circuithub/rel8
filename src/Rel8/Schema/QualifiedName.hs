{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Schema.QualifiedName
  ( QualifiedName (..)
  , ppQualifiedName
  )
where

-- base
import Data.Kind (Type)
import Data.String (IsString, fromString)
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye

-- pretty
import Text.PrettyPrint (Doc)


-- | A name of an object (such as a table, view, function or sequence)
-- qualified by an optional schema. In the absence of an explicit schema,
-- the connection's @search_path@ will be used implicitly.
type QualifiedName :: Type
data QualifiedName = QualifiedName
  { name :: String
    -- ^ The name of the object.
  , schema :: Maybe String
    -- ^ The schema that this object belongs to. If 'Nothing', whatever is on
    -- the connection's @search_path@ will be used.
   }
  deriving stock (Eq, Ord, Show)


-- | Constructs 'QualifiedName's with 'schema' set to 'Nothing'.
instance IsString QualifiedName where
  fromString name = QualifiedName {schema = Nothing, ..}


ppQualifiedName :: QualifiedName -> Doc
ppQualifiedName QualifiedName {..} = Opaleye.ppTable Opaleye.SqlTable
  { sqlTableSchemaName = schema
  , sqlTableName = name
  }
