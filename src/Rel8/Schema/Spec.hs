{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec
  ( Spec( Spec )
  , SSpec( SSpec, labels, necessity, info, nullability )
  , KnownSpec( specSing )
  , Context, KTable
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Kind.Labels ( Labels, SLabels, KnownLabels, labelsSing )
import Rel8.Kind.Necessity
  ( Necessity
  , SNecessity
  , KnownNecessity, necessitySing
  )
import Rel8.Schema.Nullability ( Nullability, Nullabilizes, nullabilization )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation )


type Spec :: Type
data Spec = Spec Labels Necessity Type Type


type SSpec :: Spec -> Type
data SSpec spec where
  SSpec ::
    { labels :: SLabels labels
    , necessity :: SNecessity necessity
    , info :: TypeInformation db
    , nullability :: Nullability db a
    }
    -> SSpec ('Spec labels necessity db a)


type KnownSpec :: Spec -> Constraint
class KnownSpec spec where
  specSing :: SSpec spec


instance
  ( KnownLabels labels
  , KnownNecessity necessity
  , DBType db
  , Nullabilizes db a
  ) => KnownSpec ('Spec labels necessity db a)
 where
  specSing = SSpec
    { labels = labelsSing
    , necessity = necessitySing
    , info = typeInformation
    , nullability = nullabilization
    }


type Context :: Type
type Context = Spec -> Type


type KTable :: Type
type KTable = Context -> Type
