{-# language DisambiguateRecordFields #-}
{-# language StandaloneKindSignatures #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Rel8.Type.JSONEncoded (
  JSONEncoded(..),
) where

-- aeson
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, parseJSON, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.Aeson.Types (parseEither)

-- base
import Data.Bifunctor (first)
import Data.Functor.Contravariant ((>$<))
import Data.Kind ( Type )
import Prelude

-- hasql
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

-- rel8
import Rel8.Type ( DBType(..) )
import Rel8.Type.Decoder (Decoder (..))
import Rel8.Type.Encoder (Encoder (..))
import Rel8.Type.Information ( TypeInformation(..) )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Default as Opaleye ( quote )

-- text
import Data.Text (pack)
import Data.Text.Lazy (unpack)


-- | A deriving-via helper type for column types that store a Haskell value
-- using a JSON encoding described by @aeson@'s 'ToJSON' and 'FromJSON' type
-- classes.
type JSONEncoded :: Type -> Type
newtype JSONEncoded a = JSONEncoded { fromJSONEncoded :: a }
  deriving (Show, Eq, Ord)


instance (FromJSON a, ToJSON a) => DBType (JSONEncoded a) where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = toJSON . fromJSONEncoded >$< Encoders.json
          , text = Aeson.fromEncoding . Aeson.toEncoding . fromJSONEncoded
          , quote =
              Opaleye.ConstExpr . Opaleye.OtherLit . Opaleye.quote .
              unpack . Aeson.encodeToLazyText . fromJSONEncoded
          }
    , decode =
        Decoder
          { binary =
              Decoders.refine
                (first pack . fmap JSONEncoded . parseEither parseJSON)
                Decoders.json
          , text = fmap JSONEncoded . eitherDecodeStrict
          }
    , delimiter = ','
    , typeName = "json"
    }
