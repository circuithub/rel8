{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Internal.Table.ADT
  ( ADTWith( ADTWith, ADT )
  , ADT
  , ADTableWith, ADTable
  , BuildableADTWith, BuildableADT
  , BuildADTWith, buildADTWith
  , BuildADT, buildADT
  , ConstructableADTWith, ConstructableADT
  , ConstructADT, constructADTWith, constructADT
  , DeconstructADTWith, deconstructADTWith, deconstructAADTWith
  , DeconstructADT, deconstructADT, deconstructAADT
  , NameADTWith, nameADTWith
  , NameADT, nameADT
  , ADTRep
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics ( Generic, from, to )
import GHC.TypeLits ( Symbol )
import Prelude

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.FCF ( Eval, Exp )
import Rel8.Internal.Generic.Construction
  ( GGBuildable
  , GGBuild, ggbuild
  , GGConstructable
  , GGConstruct, ggconstruct
  , GGDeconstruct, ggdeconstruct, ggdeconstructA
  , GGName, ggname
  )
import Rel8.Internal.Generic.Record ( Record( Record ), unrecord )
import Rel8.Internal.Generic.Rel8able
  ( Rel8able
  , GRep, GColumns, gfromColumns, gtoColumns
  , GFromExprs, gfromResult, gtoResult
  , TSerialize, deserialize, serialize
  )
import qualified Rel8.Internal.Generic.Table.ADT as G
import qualified Rel8.Internal.Kind.Algebra as K
import Rel8.Internal.Schema.HTable ( HTable )
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.Name ( Name )
import Rel8.Internal.Schema.Result ( Result )
import Rel8.Internal.Table ( Table, TColumns )
import Rel8.Internal.Type.Tag ( Tag )

-- semigroupoids
import Data.Functor.Apply (Apply)


type ADTWith :: Type -> K.Rel8able -> K.Rel8able
newtype ADTWith tag t context = ADTWith (GColumnsADT tag t context)


type ADT :: K.Rel8able -> K.Rel8able
type ADT = ADTWith Tag


pattern ADT :: GColumnsADT Tag t context -> ADT t context
pattern ADT a = ADTWith a
{-# COMPLETE ADT #-}


instance ADTableWith tag t => Rel8able (ADTWith tag t) where
  type GColumns (ADTWith tag t) = GColumnsADT tag t
  type GFromExprs (ADTWith tag t) = t Result

  gfromColumns _ = ADTWith
  gtoColumns _ (ADTWith a) = a

  gfromResult =
    unrecord .
    to .
    G.gfromResultADT
      @tag
      @TSerialize
      @TColumns
      @(Eval (ADTRep t Expr))
      @(Eval (ADTRep t Result))
      (\(_ :: proxy x) -> deserialize @_ @x)

  gtoResult =
    G.gtoResultADT
      @tag
      @TSerialize
      @TColumns
      @(Eval (ADTRep t Expr))
      @(Eval (ADTRep t Result))
      (\(_ :: proxy x) -> serialize @_ @x) .
    from .
    Record


type ADTableWith :: Type -> K.Rel8able -> Constraint
class
  ( Generic (Record (t Result))
  , HTable (GColumnsADT tag t)
  , G.GSerializeADT tag TSerialize TColumns (Eval (ADTRep t Expr)) (Eval (ADTRep t Result))
  )
  => ADTableWith tag t
instance
  ( Generic (Record (t Result))
  , HTable (GColumnsADT tag t)
  , G.GSerializeADT tag TSerialize TColumns (Eval (ADTRep t Expr)) (Eval (ADTRep t Result))
  )
  => ADTableWith tag t


type ADTable :: K.Rel8able -> Constraint
type ADTable = ADTableWith Tag


type BuildableADTWith :: Type -> K.Rel8able -> Symbol -> Constraint
class GGBuildable 'K.Sum tag name (ADTRep t) => BuildableADTWith tag t name
instance GGBuildable 'K.Sum tag name (ADTRep t) => BuildableADTWith tag t name


type BuildableADT :: K.Rel8able -> Symbol -> Constraint
type BuildableADT = BuildableADTWith Tag


type BuildADTWith :: Type -> K.Rel8able -> Symbol -> Type
type BuildADTWith tag t name = GGBuild 'K.Sum name (ADTRep t) (ADTWith tag t Expr)


type BuildADT :: K.Rel8able -> Symbol -> Type
type BuildADT t name = BuildADTWith Tag t name


buildADTWith :: forall tag t name. BuildableADTWith tag t name
  => BuildADTWith tag t name
buildADTWith =
  ggbuild @'K.Sum @tag @name @(ADTRep t) @(ADTWith tag t Expr) ADTWith


buildADT :: forall t name. BuildableADT t name => BuildADT t name
buildADT = buildADTWith @Tag @t @name


type ConstructableADTWith :: Type -> K.Rel8able -> Constraint
class GGConstructable 'K.Sum tag (ADTRep t) => ConstructableADTWith tag t
instance GGConstructable 'K.Sum tag (ADTRep t) => ConstructableADTWith tag t


type ConstructableADT :: K.Rel8able -> Constraint
type ConstructableADT = ConstructableADTWith Tag


type ConstructADT :: K.Rel8able -> Type
type ConstructADT t = forall r. GGConstruct 'K.Sum (ADTRep t) r


constructADTWith :: forall tag t. ConstructableADTWith tag t
  => ConstructADT t -> ADTWith tag t Expr
constructADTWith f =
  ggconstruct @'K.Sum @tag @(ADTRep t) @(ADTWith tag t Expr) ADTWith
    (f @(ADTWith tag t Expr))


constructADT :: forall t. ConstructableADT t => ConstructADT t -> ADT t Expr
constructADT = constructADTWith @Tag @t


type DeconstructADTWith :: Type -> K.Rel8able -> Type -> Type
type DeconstructADTWith tag t r =
  GGDeconstruct 'K.Sum (ADTRep t) (ADTWith tag t Expr) r


type DeconstructADT :: K.Rel8able -> Type -> Type
type DeconstructADT t r = DeconstructADTWith Tag t r


deconstructADTWith :: forall tag t r. (ConstructableADTWith tag t, Table Expr r)
  => DeconstructADTWith tag t r
deconstructADTWith =
  ggdeconstruct @'K.Sum @tag @(ADTRep t) @(ADTWith tag t Expr) @r
    (\(ADTWith a) -> a)


deconstructADT :: forall t r. (ConstructableADT t, Table Expr r)
  => DeconstructADT t r
deconstructADT = deconstructADTWith @Tag @t @r


deconstructAADTWith :: forall tag t f r.
  (ConstructableADTWith tag t, Apply f, Table Expr r)
  => DeconstructADTWith tag t (f r)
deconstructAADTWith =
  ggdeconstructA @'K.Sum @tag @(ADTRep t) @(ADTWith tag t Expr) @f @r
    (\(ADTWith a) -> a)


deconstructAADT :: forall t f r. (ConstructableADT t, Apply f, Table Expr r)
  => DeconstructADT t (f r)
deconstructAADT = deconstructAADTWith @Tag @t @f @r


type NameADTWith :: Type -> K.Rel8able -> Type
type NameADTWith tag t = GGName 'K.Sum tag (ADTRep t) (ADTWith tag t Name)


type NameADT :: K.Rel8able -> Type
type NameADT t = NameADTWith Tag t


nameADTWith :: forall tag t. ConstructableADTWith tag t => NameADTWith tag t
nameADTWith = ggname @'K.Sum @tag @(ADTRep t) @(ADTWith tag t Name) ADTWith


nameADT :: forall t. ConstructableADT t => NameADT t
nameADT = nameADTWith @Tag @t


data ADTRep :: K.Rel8able -> K.Context -> Exp (Type -> Type)
type instance Eval (ADTRep t context) = GRep t context


type GColumnsADT :: Type -> K.Rel8able -> K.HTable
type GColumnsADT tag t = G.GColumnsADT tag TColumns (GRep t Expr)

