{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rel8.Schema.Spec (
  Spec (Spec, labels, info, nullity),
  specification,
)
where

-- base
import Data.Kind (Type)
import Prelude

-- rel8
import Rel8.Schema.Null (Nullity, Sql, Unnullify, nullable)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Information (TypeInformation)


type Spec :: Type -> Type
data Spec a = Spec
  { labels :: [String]
  , info :: TypeInformation (Unnullify a)
  , nullity :: Nullity a
  }


specification :: Sql DBType a => Spec a
specification =
  Spec
    { labels = []
    , info = typeInformation
    , nullity = nullable
    }
