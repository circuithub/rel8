{-# language OverloadedStrings #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.JSONBEncoded
  ( JSONBEncoded(..)
  )
where

-- aeson
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, parseJSON, toJSON)
import Data.Aeson.Types (parseEither)

-- base
import Data.Bifunctor ( first )
import Data.Kind ( Type )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Type ( DBType(..) )
import Rel8.Type.Decoder (Decoder (..))
import Rel8.Type.Information ( TypeInformation(..) )

-- text
import Data.Text ( pack )


-- | Like 'Rel8.JSONEncoded', but works for @jsonb@ columns.
type JSONBEncoded :: Type -> Type
newtype JSONBEncoded a = JSONBEncoded { fromJSONBEncoded :: a }


instance (FromJSON a, ToJSON a) => DBType (JSONBEncoded a) where
  typeInformation = TypeInformation
    { encode = encode typeInformation . toJSON . fromJSONBEncoded
    , decode =
        Decoder
          { binary = Hasql.refine (first pack . fmap JSONBEncoded . parseEither parseJSON) Hasql.jsonb
          , parser = fmap JSONBEncoded . eitherDecodeStrict
          , delimiter = ','
          }
    , typeName = "jsonb"
    }
