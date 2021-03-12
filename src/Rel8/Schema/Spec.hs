{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Spec
  ( Spec( Spec, nullability, necessity, type_ )
  , SSpec( SSpec, snullability, snecessity, stype )
  , KnownSpec( specSing )
  , Context
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Kind.Necessity
  ( Necessity
  , SNecessity
  , KnownNecessity, necessitySing
  )
import Rel8.Kind.Nullability
  ( Nullability
  , SNullability
  , KnownNullability, nullabilitySing
  )
import Rel8.Type ( DBType, TypeInformation, typeInformation )


type Spec :: Type
data Spec = Spec
  { necessity :: Necessity
  , nullability :: Nullability
  , type_ :: Type
  }


type SSpec :: Spec -> Type
data SSpec spec where
  SSpec ::
    { snecessity :: SNecessity necessity
    , snullability :: SNullability nullability
    , stype :: TypeInformation a
    }
    -> SSpec ('Spec necessity nullability a)


type KnownSpec :: Spec -> Constraint
class KnownSpec spec where
  specSing :: SSpec spec


instance
  ( KnownNecessity necessity
  , KnownNullability nullability
  , DBType a
  ) => KnownSpec ('Spec necessity nullability a)
 where
  specSing = SSpec
    { snecessity = necessitySing
    , snullability = nullabilitySing
    , stype = typeInformation
    }


type Context :: Type
type Context = Spec -> Type
