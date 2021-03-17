module Rel8.DBType.JSONBEncoded ( JSONBEncoded(..) ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.DBType ( DBType( typeInformation ) )
import Rel8.DatabaseType
  ( DatabaseType( encode, decoder, typeName, DatabaseType )
  , parseDatabaseType
  )
import Rel8.DatabaseType.Decoder ( valueDecoder )


-- | Like 'JSONEncoded', but works for @jsonb@ columns.
newtype JSONBEncoded a = JSONBEncoded { fromJSONBEncoded :: a }


instance (FromJSON a, ToJSON a) => DBType (JSONBEncoded a) where
  typeInformation = parseDatabaseType f g DatabaseType
    { encode = encode typeInformation
    , decoder = valueDecoder Hasql.jsonb
    , typeName = "jsonb"
    }
    where
      f = fmap JSONBEncoded . parseEither parseJSON
      g = toJSON . fromJSONBEncoded
