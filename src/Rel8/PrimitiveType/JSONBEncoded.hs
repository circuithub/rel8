module Rel8.PrimitiveType.JSONBEncoded ( JSONBEncoded(..) ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- base
import Data.Bifunctor ( first )

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.DatabaseType ( DatabaseType( encode, decoder, typeName, DatabaseType, parser ) )
import Rel8.PrimitiveType ( PrimitiveType( typeInformation ) )

-- text
import Data.Text ( pack )


-- | Like 'JSONEncoded', but works for @jsonb@ columns.
newtype JSONBEncoded a = JSONBEncoded { fromJSONBEncoded :: a }


instance (FromJSON a, ToJSON a) => PrimitiveType (JSONBEncoded a) where
  typeInformation = DatabaseType
    { encode = encode typeInformation . toJSON . fromJSONBEncoded
    , decoder = Hasql.jsonb
    , typeName = "jsonb"
    , parser = first pack . fmap JSONBEncoded . parseEither parseJSON
    }
