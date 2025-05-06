{-# language DisambiguateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.JSONBEncoded (
  JSONBEncoded(..),
) where

-- aeson
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, parseJSON, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.Aeson.Types (parseEither)

-- base
import Data.Bifunctor ( first )
import Data.Functor.Contravariant ((>$<))
import Data.Kind ( Type )
import Prelude

-- hasql
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Default as Opaleye (quote)

-- rel8
import Rel8.Type ( DBType(..) )
import Rel8.Type.Decoder (Decoder (..))
import Rel8.Type.Encoder (Encoder (..))
import Rel8.Type.Information ( TypeInformation(..) )

-- text
import Data.Text ( pack )
import Data.Text.Lazy (unpack)


-- | Like 'Rel8.JSONEncoded', but works for @jsonb@ columns.
type JSONBEncoded :: Type -> Type
newtype JSONBEncoded a = JSONBEncoded { fromJSONBEncoded :: a }
  deriving (Show, Eq, Ord)


instance (FromJSON a, ToJSON a) => DBType (JSONBEncoded a) where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = toJSON . fromJSONBEncoded >$< Encoders.jsonb
          , text = Aeson.fromEncoding . Aeson.toEncoding . fromJSONBEncoded
          , quote =
              Opaleye.ConstExpr . Opaleye.OtherLit .
              Opaleye.quote .
              unpack . Aeson.encodeToLazyText . fromJSONBEncoded
          }
    , decode =
        Decoder
          { binary =
              Decoders.refine
                (first pack . fmap JSONBEncoded . parseEither parseJSON)
                Decoders.jsonb
          , text = fmap JSONBEncoded . eitherDecodeStrict
          }
    , delimiter = ','
    , typeName = "jsonb"
    }
