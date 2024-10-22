{-# language StandaloneKindSignatures #-}
{-# language OverloadedStrings        #-}
{-# language TypeApplications         #-}

module Rel8.Type.JSONEncoded ( JSONEncoded(..) ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, parseJSON )
import Data.Aeson.Types ( parseEither )
import qualified Data.Aeson as Aeson

-- base
import Data.Kind ( Type )
import Data.Bifunctor (first)
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Type ( DBType(..) )
import Rel8.Type.Information ( TypeInformation(..) )
import Rel8.Type.Decoder ( Decoder(..) )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Default as Opaleye ( quote )

-- text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy


-- | A deriving-via helper type for column types that store a Haskell value
-- using a JSON encoding described by @aeson@'s 'ToJSON' and 'FromJSON' type
-- classes.
type JSONEncoded :: Type -> Type
newtype JSONEncoded a = JSONEncoded { fromJSONEncoded :: a }
  deriving (Show, Eq, Ord)


instance (FromJSON a, ToJSON a) => DBType (JSONEncoded a) where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit . Opaleye.quote .
        Lazy.unpack . Lazy.decodeUtf8 .
        Aeson.encode . fromJSONEncoded
    , decode =
        Decoder
          { binary = Hasql.refine (first Text.pack . fmap JSONEncoded . parseEither parseJSON) Hasql.json
          , parser = fmap JSONEncoded . Aeson.eitherDecodeStrict
          , delimiter = ','
          }
    , typeName = "json"
    }
