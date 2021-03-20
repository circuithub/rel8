module Rel8.DBType.JSONBEncoded ( JSONBEncoded(..) ) where

-- 
import qualified Hasql.Decoders as Hasql

-- aeson
import Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- rel8
import Rel8.DBType ( DBType( typeInformation ) )
import Rel8.DatabaseType
  ( DatabaseType( encode, decoder, typeName, DatabaseType, parser )
  , parseDatabaseType
  )


-- | Like 'JSONEncoded', but works for @jsonb@ columns.
newtype JSONBEncoded a = JSONBEncoded { fromJSONBEncoded :: a }


instance (FromJSON a, ToJSON a) => DBType (JSONBEncoded a) where
  typeInformation = parseDatabaseType f g DatabaseType
    { encode = encode typeInformation
    , decoder = Hasql.jsonb
    , typeName = "jsonb"
    , parser = pure
    }
    where
      f = fmap JSONBEncoded . parseEither parseJSON
      g = toJSON . fromJSONBEncoded
