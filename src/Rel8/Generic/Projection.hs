{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Generic.Projection
  (
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics ( Rep, (:*:), K1, M1, Meta( MetaData, MetaSel ), C, D, S )
import GHC.OverloadedLabels ( IsLabel, fromLabel )
import GHC.TypeLits ( Symbol, TypeError, ErrorMessage( (:<>:), Text ) )
import Prelude

-- rel8
import Rel8.Category.Projection ( Projection( Projection ) )
import Rel8.Generic.Record ( Record )
import Rel8.Generic.Table.Record ( GColumns )
import Rel8.Schema.HTable.Label ( hunlabel )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import Rel8.Table ( Columns, Table, TColumns )


type GField :: Symbol -> (Type -> Type) -> Type
type family GField name rep where
  GField name (M1 D ('MetaData datatype _ _ _) rep) =
    GField' name rep (TypeError (NoSelector datatype name))


type GField' :: Symbol -> (Type -> Type) -> Type -> Type
type family GField' name rep fallback where
  GField' name (M1 C _ rep) fallback = GField' name rep fallback
  GField' name (a :*: b) fallback = GField' name a (GField' name b fallback)
  GField' name (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ a)) _ = a
  GField' _ _ fallback = fallback


type NoSelector :: Symbol -> Symbol -> ErrorMessage
type NoSelector datatype selector =
  ( 'Text "The type `" ':<>:
    'Text datatype ':<>:
    'Text "` has no field `" ':<>:
    'Text selector ':<>:
    'Text "`."
  )


type GProjection :: Symbol -> (Type -> Type) -> Constraint
class GProjection name rep where
  gprojection :: GColumns TColumns rep context -> Columns (GField name rep) context


instance
  ( GProjection' name rep fallback
  , fallback ~ TypeError (NoSelector datatype name)
  )
  => GProjection name (M1 D ('MetaData datatype _m _p _nt) rep)
 where
  gprojection a = gprojection' @name @rep @fallback a fallback
    where
      fallback = error "gprojection: impossible!"


type GProjection' :: Symbol -> (Type -> Type) -> Type -> Constraint
class GProjection' name rep fallback where
  gprojection' :: ()
    => GColumns TColumns rep context
    -> Columns fallback context
    -> Columns (GField' name rep fallback) context


instance GProjection' name rep fallback => GProjection' name (M1 C meta rep) fallback where
  gprojection' = gprojection' @name @rep @fallback


instance
  ( GProjection' name a fallback'
  , GProjection' name b fallback
  , fallback' ~ GField' name b fallback
  )
  => GProjection' name (a :*: b) fallback
 where
  gprojection' (HProduct a b) fallback =
    gprojection' @name @a @fallback' a $
      gprojection' @name @b @fallback b fallback


instance Table context a =>
  GProjection' name (M1 S ('MetaSel ('Just name) _su _ss _ds) (K1 i a)) fallback
 where
  gprojection' a _ = hunlabel a


instance {-# OVERLAPPABLE #-} GField' name rep fallback ~ fallback =>
  GProjection' name rep fallback
 where
  gprojection' _ = id


instance
  ( rep ~ Rep (Record a)
  , Columns a ~ GColumns TColumns rep
  , GProjection name rep
  , b ~ GField name rep
  )
  => IsLabel name (Projection a b)
 where
  fromLabel = Projection $ gprojection @name @rep
