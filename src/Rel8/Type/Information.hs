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


type TypeInformation :: Type -> Type
data TypeInformation a = TypeInformation
  { encode :: a -> Opaleye.PrimExpr
  , decode :: Hasql.Value a
  , typeName :: String
  }


mapTypeInformation :: ()
  => (a -> b) -> (b -> a)
  -> TypeInformation a -> TypeInformation b
mapTypeInformation = parseTypeInformation . fmap pure


parseTypeInformation :: ()
  => (a -> Either String b) -> (b -> a)
  -> TypeInformation a -> TypeInformation b
parseTypeInformation to from TypeInformation {encode, decode, typeName} =
  TypeInformation
    { encode = encode . from
    , decode = Hasql.refine (first Text.pack . to) decode
    , typeName
    }
