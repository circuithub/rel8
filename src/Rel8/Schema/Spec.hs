{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec
  ( Spec( Spec, nullability, necessity, blueprint )
  , SSpec( SSpec, snullability, snecessity, sblueprint, stypeInformation )
  , KnownSpec( specSing )
  , Context
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Kind.Blueprint
  ( Blueprint
  , SBlueprint
  , KnownBlueprint, blueprintSing
  , ToDBType
  )
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
  , blueprint :: Blueprint
  }


type SSpec :: Spec -> Type
data SSpec spec where
  SSpec ::
    { snecessity :: SNecessity necessity
    , snullability :: SNullability nullability
    , sblueprint :: SBlueprint blueprint
    , stypeInformation :: TypeInformation (ToDBType blueprint)
    }
    -> SSpec ('Spec necessity nullability blueprint)


type KnownSpec :: Spec -> Constraint
class KnownSpec spec where
  specSing :: SSpec spec


instance
  ( KnownNecessity necessity
  , KnownNullability nullability
  , KnownBlueprint blueprint
  , DBType (ToDBType blueprint)
  ) => KnownSpec ('Spec necessity nullability blueprint)
 where
  specSing = SSpec
    { snecessity = necessitySing
    , snullability = nullabilitySing
    , sblueprint = blueprintSing
    , stypeInformation = typeInformation
    }


type Context :: Type
type Context = Spec -> Type
