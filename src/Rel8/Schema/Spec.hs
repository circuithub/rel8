{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec
  ( Spec( Spec )
  , SSpec( SSpec, labels, info, nullity )
  , KnownSpec( specSing )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Schema.Null ( Nullity, Sql, Unnullify, nullable )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation )


type Spec :: Type
newtype Spec = Spec Type


type SSpec :: Spec -> Type
data SSpec spec where
  SSpec ::
    { labels :: [String]
    , info :: TypeInformation (Unnullify a)
    , nullity :: Nullity a
    }
    -> SSpec ('Spec a)


type KnownSpec :: Spec -> Constraint
class KnownSpec spec where
  specSing :: SSpec spec


instance Sql DBType a => KnownSpec ('Spec a) where
  specSing = SSpec
    { labels = []
    , info = typeInformation
    , nullity = nullable
    }
