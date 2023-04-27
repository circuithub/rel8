{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language DisambiguateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Main ( main ) where

import Data.Functor.Contravariant ( (>$<) )
import Data.Maybe ( fromMaybe )
import qualified Data.ByteString.Char8 as BS
import Options.Applicative ( Parser, strOption, long, info, execParser )
import Data.ByteString ( ByteString )
import Data.Int ( Int64 )
import Data.Text ( Text, unpack )
import GHC.Generics ( Generic )
import Hasql.Connection
import Prelude hiding ( filter )
import Rel8 hiding (Table)
import Text.Casing ( camel, pascal )
import qualified Language.Haskell.Exts.Pretty as HS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Data.List.NonEmpty as NonEmpty

data Relkind = RTable
  deriving stock (Show)
  deriving anyclass (DBEq)

instance DBType Relkind where
  typeInformation = parseTypeInformation parser printer typeInformation
    where
      parser = \case
        "r"         -> pure RTable
        (x :: Text) -> Left $ "Unknown relkind: " ++ show x

      printer = \case
        RTable -> "r"

newtype Oid = Oid Int64
  deriving newtype (DBType, DBEq, Show)

data PGClass f = PGClass
  { oid :: Column f Oid
  , relname :: Column f Text
  , relkind :: Column f Relkind
  , relnamespace :: Column f Oid
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (PGClass f)

pgclass :: TableSchema (PGClass Name)
pgclass = TableSchema
  { name = "pg_class"
  , schema = Just "pg_catalog"
  , columns = namesFromLabelsWith NonEmpty.last
  }

data PGAttribute f = PGAttribute
  { attrelid :: Column f Oid
  , attname :: Column f Text
  , atttypid :: Column f Oid
  , attnum :: Column f Int64
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (PGAttribute f)

pgattribute :: TableSchema (PGAttribute Name)
pgattribute = TableSchema
  { name = "pg_attribute"
  , schema = Just "pg_catalog"
  , columns = namesFromLabelsWith NonEmpty.last
  }

data PGType f = PGType
  { oid :: Column f Oid
  , typname :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (PGType f)

pgtype :: TableSchema (PGType Name)
pgtype = TableSchema
  { name = "pg_type"
  , schema = Just "pg_catalog"
  , columns = namesFromLabelsWith NonEmpty.last
  }

data PGNamespace f = PGNamespace
  { oid :: Column f Oid
  , nspname :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (PGNamespace f)

pgnamespace :: TableSchema (PGNamespace Name)
pgnamespace = TableSchema
  { name = "pg_namespace"
  , schema = Just "pg_catalog"
  , columns = namesFromLabelsWith NonEmpty.last
  }

data Table f = Table
  { name :: Column f Text
  , columns :: HList f (Attribute f)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Table f)

data Attribute f = Attribute
  { attribute :: PGAttribute f
  , typ :: PGType f
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Attribute f)


data Arguments = Arguments
  { connectionString :: ByteString
  , schema :: Text
  }

parser :: Parser Arguments
parser = do
  connectionString <- BS.pack <$> strOption (long "connection")
  schema <- strOption (long "schema")
  return Arguments{..}

main :: IO ()
main = do
  Arguments{ connectionString, schema } <- execParser $ info parser mempty
  c <- acquire connectionString >>= either (fail . show) return

  tables <- select c do
    table@PGClass{ oid = tableOid, relname, relnamespace } <- orderBy (relname >$< asc) do
      each pgclass
        >>= filter ((lit RTable ==.) . relkind)

    namespace <-
      each pgnamespace
        >>= filter (\PGNamespace{ oid = namespaceOid } -> relnamespace ==. namespaceOid)
        >>= filter ((lit schema ==.) . nspname)

    columns <- many do
      attribute@PGAttribute{ atttypid } <-
        each pgattribute
          >>= filter ((tableOid ==.) . attrelid)
          >>= filter ((>. 0) . attnum)

      typ <-
        each pgtype
          >>= filter (\PGType{ oid = typoid } -> atttypid ==. typoid)

      return Attribute{ attribute, typ }

    return Table
      { name = relname
      , ..
      }

  putStrLn $ HS.prettyPrint $ tablesToModule tables


tablesToModule :: [Table Result] -> HS.Module ()
tablesToModule tables = HS.Module () Nothing pragmas imports allTableDecls
  where
    pragmas = [ deriveGeneric, deriveAnyClass, derivingStrategies, overloadedStrings ]
      where
        deriveGeneric = HS.LanguagePragma () $ pure $ HS.Ident () "DeriveGeneric"
        deriveAnyClass = HS.LanguagePragma () $ pure $ HS.Ident () "DeriveAnyClass"
        derivingStrategies = HS.LanguagePragma () $ pure $ HS.Ident () "DerivingStrategies"
        overloadedStrings = HS.LanguagePragma () $ pure $ HS.Ident () "OverloadedStrings"

    imports = map mkImport [ "GHC.Generic", "Rel8" ]
      where
        mkImport name = HS.ImportDecl
          { importAnn = ()
          , importModule = HS.ModuleName () name
          , importQualified = False
          , importSrc = False
          , importSafe = False
          , importPkg = Nothing
          , importAs = Nothing
          , importSpecs = Nothing
          }

    allTableDecls = concatMap tableDecls tables
      where
        tableDecls Table{ name, columns } = rel8able:tableSchema
          where
            pascalName = HS.Ident () $ pascal $ unpack name

            rel8able = HS.DataDecl () (HS.DataType ()) Nothing declHead [constructor] [derivingGeneric, derivingRel8able]
              where
                declHead = HS.DHApp () tyName f
                  where
                    tyName = HS.DHead () pascalName
                    f = HS.UnkindedVar () (HS.Ident () "f")

                constructor = HS.QualConDecl () Nothing Nothing conDecl
                  where
                    conDecl = HS.RecDecl () pascalName $ map field columns
                    field Attribute{ attribute, typ } = HS.FieldDecl () [fieldName] $ columnF columnType
                      where
                        fieldName = HS.Ident () $ camel $ unpack $ attname attribute

                        columnType =
                          HS.TyCon () $ HS.UnQual () $ HS.Ident () $
                            maybe (pascal $ unpack $ typname typ) unpack (lookup (typname typ) typeMapping)

                        columnF = HS.TyApp () (HS.TyApp () _Column f)
                          where
                            _Column = HS.TyCon () $ HS.UnQual () $ HS.Ident () "Column"
                            f = HS.TyVar () $ HS.Ident () "f"

                derivingGeneric = HS.Deriving () (Just (HS.DerivStock ())) [rule]
                  where
                    rule = HS.IRule () Nothing Nothing $ HS.IHCon () $ HS.UnQual () $ HS.Ident () "Generic"

                derivingRel8able = HS.Deriving () (Just (HS.DerivAnyclass ())) [rule]
                  where
                    rule = HS.IRule () Nothing Nothing $ HS.IHCon () $ HS.UnQual () $ HS.Ident () "Rel8able"

            tableSchema = [ typeSig, HS.FunBind () [match] ]
              where
                schemaName = HS.Ident () $ camel $ unpack name

                typeSig = HS.TypeSig () [schemaName] t
                  where
                    t = HS.TyApp () _TableSchema (HS.TyApp () (HS.TyCon () (HS.UnQual () pascalName)) _Name)
                      where
                        _TableSchema = HS.TyCon () $ HS.UnQual () $ HS.Ident () "TableSchema"
                        _Name = HS.TyCon () $ HS.UnQual () $ HS.Ident () "Name"

                match = HS.Match () schemaName [] rhs Nothing
                  where
                    rhs = HS.UnGuardedRhs () tableSchemaExp
                      where
                        tableSchemaExp = HS.RecConstr () _TableSchema [ nameField, schemaField, columnsField ]
                          where
                            _TableSchema = HS.UnQual () $ HS.Ident () "TableSchema"

                            nameField = HS.FieldUpdate () (HS.UnQual () (HS.Ident () "name")) $ HS.Lit () $ HS.String () str str
                              where
                                str = unpack name

                            schemaField =
                              HS.FieldUpdate () (HS.UnQual () (HS.Ident () "schema")) $
                                HS.App () (HS.Con () (HS.UnQual () (HS.Ident () "Just"))) $
                                  HS.Lit () $ HS.String () str str
                              where
                                str = unpack name

                            columnsField = HS.FieldUpdate () columnsName columnsRecord
                              where
                                columnsName = HS.UnQual () (HS.Ident () "columns")
                                columnsRecord = HS.RecConstr () (HS.UnQual () pascalName) $ map field columns

                    field Attribute{ attribute, typ } = HS.FieldUpdate () (HS.UnQual () fieldName) columnName
                      where
                        fieldName = HS.Ident () $ camel $ unpack $ attname attribute
                        columnName = HS.Lit () $ HS.String () str str
                          where
                            str = unpack $ attname attribute


typeMapping :: [(Text, Text)]
typeMapping =
  [ ("bool", "Bool")
  , ("char", "Char")
  , ("float8", "Double")
  , ("int2", "Int16")
  , ("int4", "Int32")
  , ("int8", "Int64")
  , ("bytea", "ByteString")
  , ("numeric", "Scientific")
  , ("text", "Text")
  , ("varchar", "Text")
  , ("timestamptz", "UTCTime")
  , ("jsonb", "Value")
  , ("uuid", "UUID")
  , ("interval", "CalendarDiffTime")
  , ("date", "Day")
  , ("time", "TimeOfDay")
  , ("timestamp", "LocalTime")
  , ("citext", "CI Text")
  ]
