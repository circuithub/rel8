{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
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
  , Interpretation( Col )
  , Col( Result )
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Prelude ( Monoid( mempty ), Semigroup( (<>) ) )

-- rel8
import Rel8.Kind.Labels ( Labels, SLabels, KnownLabels, labelsSing )
import Rel8.Kind.Necessity
  ( Necessity
  , SNecessity
  , KnownNecessity, necessitySing
  )
import Rel8.Schema.Nullability ( Unnullify, Nullability, Sql, nullabilization )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation )


type Interpretation :: (Type -> Type) -> Constraint
class Interpretation f where
  data Col f :: Spec -> Type 


instance Interpretation Identity where
  data Col Identity :: Spec -> Type where
    Result :: a -> Col Identity ('Spec labels necessity dbType a)


instance (spec ~ 'Spec labels necessity dbType a, Semigroup a) =>
  Semigroup (Col Identity spec)
 where
  Result a <> Result b = Result (a <> b)


instance (spec ~ 'Spec labels necessity dbType a, Monoid a) =>
  Monoid (Col Identity spec)
 where
  mempty = Result mempty


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


type SpecF :: Type -> Type
data SpecF a


instance Interpretation SpecF where
  data Col SpecF :: Spec -> Type where
    SpecCol :: SSpec spec -> Col SpecF spec



type KnownSpec :: Spec -> Constraint
class KnownSpec spec where
  specSing :: SSpec spec


instance
  ( KnownLabels labels
  , KnownNecessity necessity
  , Sql DBType a
  , db ~ Unnullify a
  )
  => KnownSpec ('Spec labels necessity db a)
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
