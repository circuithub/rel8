{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec
  ( Spec( Spec, labels, nullability, necessity, blueprint )
  , SSpec( SSpec, slabels, snullability, snecessity, sblueprint, stypeInformation )
  , KnownSpec( specSing )
  , Context, KTable
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
import Rel8.Kind.Labels ( Labels, SLabels, KnownLabels, labelsSing )
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
  { labels :: Labels
  , necessity :: Necessity
  , nullability :: Nullability
  , blueprint :: Blueprint
  }


type SSpec :: Spec -> Type
data SSpec spec where
  SSpec ::
    { slabels :: SLabels labels
    , snecessity :: SNecessity necessity
    , snullability :: SNullability nullability
    , sblueprint :: SBlueprint blueprint
    , stypeInformation :: TypeInformation (ToDBType blueprint)
    }
    -> SSpec ('Spec labels necessity nullability blueprint)


type KnownSpec :: Spec -> Constraint
class KnownSpec spec where
  specSing :: SSpec spec


instance
  ( KnownLabels labels
  , KnownNecessity necessity
  , KnownNullability nullability
  , KnownBlueprint blueprint
  , DBType (ToDBType blueprint)
  ) => KnownSpec ('Spec labels necessity nullability blueprint)
 where
  specSing = SSpec
    { slabels = labelsSing
    , snecessity = necessitySing
    , snullability = nullabilitySing
    , sblueprint = blueprintSing
    , stypeInformation = typeInformation
    }


type Context :: Type
type Context = Spec -> Type


type KTable :: Type
type KTable = Context -> Type
