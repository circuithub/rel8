{-# language GADTs #-}
{-# language KindSignatures #-}
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

-- base
import Control.Monad ( (>=>) )
import Data.Bifunctor ( first )
import Data.Data ( Proxy( Proxy ) )
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Opaleye ( Column, IsSqlType, showSqlType )
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- text
import Data.Text ( Text, pack )


-- | A @DatabaseType@ describes how to encode and decode a Haskell type to and
-- from database queries. The @typeName@ is the name of the type in the
-- database, which is used to accurately type literals. 
data DatabaseType :: Type -> Type where
  DatabaseType ::
    { encode :: b -> Opaleye.PrimExpr
      -- ^ How to encode a single Haskell value as a SQL expression.
    , typeName :: String
      -- ^ The name of the SQL type.
    , decoder :: Hasql.Value a
      -- ^ How to deserialize a single result back to Haskell.
    , parser :: a -> Either Text b
      -- ^ How to parse SQL values.
    } -> DatabaseType b


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
parseDatabaseType aToB bToA DatabaseType{ encode, typeName, decoder, parser } = DatabaseType
  { encode = encode . bToA
  , decoder = decoder
  , typeName
  , parser = parser >=> fmap (first pack) aToB
  }


-- | Simultaneously map over how a type is both encoded and decoded, while
-- retaining the name of the type. This operation is useful if you want to
-- essentially @newtype@ another 'DatabaseType'.
-- 
-- The mapping is required to be total. If you have a partial mapping, see
-- 'parseDatabaseType'.
mapDatabaseType :: (a -> b) -> (b -> a) -> DatabaseType a -> DatabaseType b
mapDatabaseType aToB = parseDatabaseType (pure . aToB)


fromOpaleye :: forall a b. IsSqlType b
  => (a -> Opaleye.Column b)
  -> Hasql.Value a
  -> DatabaseType a
fromOpaleye f decoder =
  DatabaseType
    { encode = \x -> case f x of Opaleye.Column e -> e
    , decoder = decoder
    , typeName = showSqlType (Proxy @b)
    , parser = pure
    }


nonEmptyNotNull :: DatabaseType a -> DatabaseType (NonEmpty a)
nonEmptyNotNull DatabaseType{ encode, typeName, decoder, parser } = DatabaseType
  { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map encode . toList
  , decoder = compositeArrayOf $ Hasql.nonNullable $ Hasql.refine parser decoder
  , typeName = "record"
  , parser = parseNonEmpty
  }


nonEmptyNull :: DatabaseType a -> DatabaseType (NonEmpty (Maybe a))
nonEmptyNull DatabaseType{ encode, typeName, decoder, parser } = DatabaseType
  { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map (maybe nullExpr encode) . toList
  , decoder = compositeArrayOf $ Hasql.nullable $ Hasql.refine parser decoder
  , typeName = "record"
  , parser = parseNonEmpty
  }


parseNonEmpty :: [a] -> Either Text (NonEmpty a)
parseNonEmpty = \case
  []   -> Left "Unexpected empty list"
  x:xs -> Right (x :| xs)


listOfNotNull :: DatabaseType a -> DatabaseType [a]
listOfNotNull DatabaseType{ encode, typeName, decoder, parser } = DatabaseType
  { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map encode . toList
  , decoder = compositeArrayOf $ Hasql.nonNullable $ Hasql.refine parser decoder
  , typeName = "record"
  , parser = pure
  }


listOfNull :: DatabaseType a -> DatabaseType [Maybe a]
listOfNull DatabaseType{ encode, typeName, decoder, parser } = DatabaseType
  { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map (maybe nullExpr encode) . toList
  , decoder = compositeArrayOf $ Hasql.nullable $ Hasql.refine parser decoder
  , typeName = "record"
  , parser = pure
  }


nullExpr :: Opaleye.PrimExpr
nullExpr = Opaleye.ConstExpr Opaleye.NullLit


compositeArrayOf :: Hasql.NullableOrNot Hasql.Value a -> Hasql.Value [a]
compositeArrayOf = Hasql.composite . Hasql.field . Hasql.nonNullable . Hasql.listArray
