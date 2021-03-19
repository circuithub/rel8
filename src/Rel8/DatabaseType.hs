{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.DatabaseType
  ( DatabaseType(..)
  , mapDatabaseType
  , parseDatabaseType
  , fromOpaleye
  , nonEmptyNotNull
  , nonEmptyNull
  , listOfNull
  , listOfNotNull
  ) where

-- 
import qualified Hasql.Decoders as Hasql

-- base
import Data.Bifunctor ( first )
import Data.Data ( Proxy( Proxy ) )
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )

-- rel8
import Opaleye ( Column, IsSqlType, showSqlType )
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- text
import Data.Text ( pack )


-- | A @DatabaseType@ describes how to encode and decode a Haskell type to and
-- from database queries. The @typeName@ is the name of the type in the
-- database, which is used to accurately type literals. 
type DatabaseType :: Type -> Type


data DatabaseType a = DatabaseType
  { encode :: a -> Opaleye.PrimExpr
    -- ^ How to encode a single Haskell value as a SQL expression.
  , typeName :: String
    -- ^ The name of the SQL type.
  , decoder :: Hasql.Value a
    -- ^ How to deserialize a single result back to Haskell.
  }


-- | Apply a parser to a 'DatabaseType'.
-- 
-- This can be used if the data stored in the database should only be subset of
-- a given 'DatabaseType'. The parser is applied when deserializing rows
-- returned - the encoder assumes that the input data is already in the
-- appropriate form.
-- 
-- One example where this may be useful is with a database that stores data in
-- some legacy encoding:
-- 
-- >>> data Color = Red | Green | Blue
-- >>> :{
-- instance DBType Color where
--   typeInformation = parseDatabaseType parseLegacy toLegacy typeInformation
--     where
--       parseLegacy :: Text -> Either String Color
--       parseLegacy "red"   = Right Red
--       parseLegacy "green" = Right Green
--       parseLegacy _       = Left "Unexpected Color"
--       toLegacy Red   = "red"
--       toLegacy Green = "green"
-- :}
parseDatabaseType :: (a -> Either String b) -> (b -> a) -> DatabaseType a -> DatabaseType b
parseDatabaseType aToB bToA DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = encode . bToA
  , decoder = Hasql.refine (first pack . aToB) decoder
  , typeName
  }


-- | Simultaneously map over how a type is both encoded and decoded, while
-- retaining the name of the type. This operation is useful if you want to
-- essentially @newtype@ another 'DatabaseType'.
-- 
-- The mapping is required to be total. If you have a partial mapping, see
-- 'parseDatabaseType'.
mapDatabaseType :: (a -> b) -> (b -> a) -> DatabaseType a -> DatabaseType b
mapDatabaseType aToB bToA DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = encode . bToA
  , decoder = aToB <$> decoder
  , typeName
  }


fromOpaleye :: forall a b. IsSqlType b
  => (a -> Opaleye.Column b)
  -> Hasql.Value a
  -> DatabaseType a
fromOpaleye f decoder =
  DatabaseType
    { encode = \x -> case f x of Opaleye.Column e -> e
    , decoder
    , typeName = showSqlType (Proxy @b)
    }


nonEmptyNotNull :: DatabaseType a -> DatabaseType (NonEmpty a)
nonEmptyNotNull DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map encode . toList
  , decoder = Hasql.refine parse $ compositeArrayOf $ Hasql.nonNullable decoder
  , typeName = "record"
  }
  where
    parse = \case
      []   -> Left "Unexpected empty list"
      x:xs -> Right (x :| xs)

    compositeArrayOf =
      Hasql.composite . Hasql.field . Hasql.nonNullable . Hasql.listArray


nonEmptyNull :: DatabaseType a -> DatabaseType (NonEmpty (Maybe a))
nonEmptyNull DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map (maybe nullExpr encode) . toList
  , decoder = Hasql.refine parse $ compositeArrayOf $ Hasql.nullable decoder
  , typeName = "record"
  }
  where
    nullExpr = Opaleye.ConstExpr Opaleye.NullLit

    parse = \case
      []   -> Left "Unexpected empty list"
      x:xs -> Right (x :| xs)

    compositeArrayOf =
      Hasql.composite . Hasql.field . Hasql.nonNullable . Hasql.listArray


listOfNotNull :: DatabaseType a -> DatabaseType [a]
listOfNotNull DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map encode . toList
  , decoder = compositeArrayOf $ Hasql.nonNullable decoder
  , typeName = "record"
  }
  where
    compositeArrayOf =
      Hasql.composite . Hasql.field . Hasql.nonNullable . Hasql.listArray


listOfNull :: DatabaseType a -> DatabaseType [Maybe a]
listOfNull DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map (maybe nullExpr encode) . toList
  , decoder = compositeArrayOf $ Hasql.nullable decoder
  , typeName = "record"
  }
  where
    nullExpr = Opaleye.ConstExpr Opaleye.NullLit

    compositeArrayOf =
      Hasql.composite . Hasql.field . Hasql.nonNullable . Hasql.listArray
