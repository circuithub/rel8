module Rel8.DBType.JSONEncoded ( JSONEncoded(..) ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- rel8
import Rel8.DBType ( DBType( typeInformation ) )
import Rel8.DatabaseType ( parseDatabaseType )


-- | A deriving-via helper type for column types that store a Haskell value
-- using a JSON encoding described by @aeson@'s 'ToJSON' and 'FromJSON' type
-- classes.
-- 
-- The declaration:
-- 
-- >>> import Data.Aeson
-- 
-- >>> :{
-- data Pet = Pet { petName :: String, petAge :: Int }
--   deriving (Generic, ToJSON, FromJSON)
--   deriving DBType via JSONEncoded Pet
-- :}
-- 
-- will allow you to store @Pet@ values in a single SQL column (stored as
-- @json@ values).
newtype JSONEncoded a = JSONEncoded { fromJSONEncoded :: a }


instance (FromJSON a, ToJSON a) => DBType (JSONEncoded a) where
  typeInformation = parseDatabaseType f g typeInformation
    where
      f = fmap JSONEncoded . parseEither parseJSON
      g = toJSON . fromJSONEncoded
