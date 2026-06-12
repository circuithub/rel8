{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}
{-# language DuplicateRecordFields #-}

module Rel8.Type.Encoder (
  Encoder (..),
) where

-- base
import Data.Functor.Contravariant (Contravariant, (>$<), contramap)
import Data.Kind (Type)
import Prelude

-- bytestring
import Data.ByteString.Builder (Builder)

-- hasql
import qualified Hasql.Encoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


type Encoder :: Type -> Type
data Encoder a = Encoder
  { binary :: Hasql.Value a
    -- ^ How to serialize to PostgreSQL's binary format.
  , text :: a -> Builder
    -- ^ How to serialize to PostgreSQL's text format.
  , quote :: a -> Opaleye.PrimExpr
    -- ^ How to encode a single Haskell value as an SQL expression.
  }


instance Contravariant Encoder where
  contramap f Encoder {..} = Encoder
    { binary = f >$< binary
    , text = text . f
    , quote = quote . f
    }
