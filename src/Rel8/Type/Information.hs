{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.Information
  ( TypeInformation(..)
  , mapTypeInformation
  , parseTypeInformation
  )
where

-- base
import Data.Bifunctor ( first )
import Data.Kind ( Type )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- text
import qualified Data.Text as Text


-- | @TypeInformation@ describes how to encode and decode a Haskell type to and
-- from database queries. The @typeName@ is the name of the type in the
-- database, which is used to accurately type literals. 
type TypeInformation :: Type -> Type
data TypeInformation a = TypeInformation
  { encode :: a -> Opaleye.PrimExpr
    -- ^ How to encode a single Haskell value as a SQL expression.
  , decode :: Hasql.Value a
    -- ^ How to deserialize a single result back to Haskell.
  , typeName :: String
    -- ^ The name of the SQL type.
  }


-- | Simultaneously map over how a type is both encoded and decoded, while
-- retaining the name of the type. This operation is useful if you want to
-- essentially @newtype@ another 'DBType'.
-- 
-- The mapping is required to be total. If you have a partial mapping, see
-- 'parseTypeInformation'.
mapTypeInformation :: ()
  => (a -> b) -> (b -> a)
  -> TypeInformation a -> TypeInformation b
mapTypeInformation = parseTypeInformation . fmap pure


-- | Apply a parser to 'TypeInformation'.
-- 
-- This can be used if the data stored in the database should only be subset of
-- a given 'TypeInformation'. The parser is applied when deserializing rows
-- returned - the encoder assumes that the input data is already in the
-- appropriate form.
-- 
-- One example where this may be useful is with a database that stores data in
-- some legacy encoding:
-- 
-- >>> import Data.Text (Text)
-- 
-- >>> data Color = Red | Green | Blue
-- >>> :{
-- instance DBType Color where
--   typeInformation = parseTypeInformation parseLegacy toLegacy typeInformation
--     where
--       parseLegacy :: Text -> Either String Color
--       parseLegacy "red"   = Right Red
--       parseLegacy "green" = Right Green
--       parseLegacy _       = Left "Unexpected Color"
--       toLegacy Red   = "red"
--       toLegacy Green = "green"
-- :}
parseTypeInformation :: ()
  => (a -> Either String b) -> (b -> a)
  -> TypeInformation a -> TypeInformation b
parseTypeInformation to from TypeInformation {encode, decode, typeName} =
  TypeInformation
    { encode = encode . from
    , decode = Hasql.refine (first Text.pack . to) decode
    , typeName
    }
