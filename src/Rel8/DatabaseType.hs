{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.DatabaseType
  ( DatabaseType(..)
  , mapDatabaseType
  , parseDatabaseType
  , fromOpaleye
  , nullDatabaseType
  ) where

-- base
import Data.Data ( Proxy( Proxy ) )
import Data.Kind ( Type )

-- rel8
import Opaleye ( Column, IsSqlType, showSqlType )
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DatabaseType.Decoder ( Decoder, acceptNull, parseDecoder )


-- | A @DatabaseType@ describes how to encode and decode a Haskell type to and
-- from database queries. The @typeName@ is the name of the type in the
-- database, which is used to accurately type literals. 
type DatabaseType :: Type -> Type


data DatabaseType a = DatabaseType
  { encode :: a -> Opaleye.PrimExpr
    -- ^ How to encode a single Haskell value as a SQL expression.
  , typeName :: String
    -- ^ The name of the SQL type.
  , decoder :: Decoder a
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
  , decoder = parseDecoder aToB decoder
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
  -> Decoder a
  -> DatabaseType a
fromOpaleye f decoder =
  DatabaseType
    { encode = \x -> case f x of Opaleye.Column e -> e
    , decoder
    , typeName = showSqlType (Proxy @b)
    }


nullDatabaseType :: DatabaseType a -> DatabaseType (Maybe a)
nullDatabaseType DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = maybe (Opaleye.ConstExpr Opaleye.NullLit) encode
  , decoder = acceptNull decoder
  , typeName
  }
