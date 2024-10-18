{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Schema.QualifiedName
  ( QualifiedName (..)
  , ppQualifiedName
  , showQualifiedName
  , showQualifiedOperator
  )
where

-- base
import Data.Kind (Type)
import Data.String (IsString, fromString)
import Prelude

-- pretty
import Text.PrettyPrint (Doc, parens, text)

-- rel8
import Rel8.Schema.Escape (escape)


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
ppQualifiedName QualifiedName {schema = mschema, ..} = case mschema of
  Nothing -> name'
  Just schema -> escape schema <> text "." <> name'
  where
    name' = escape name


showQualifiedName :: QualifiedName -> String
showQualifiedName = show . ppQualifiedName


showQualifiedOperator :: QualifiedName -> String
showQualifiedOperator QualifiedName {schema = mschema, ..} = case mschema of
  Nothing -> name
  Just schema ->
    show $ text "OPERATOR" <> parens (escape schema <> text "." <> text name)
