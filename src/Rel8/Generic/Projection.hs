{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Generic.Projection
  ( Fanoutable
  , Fanout
  , fanout
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
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Construction.Record
  ( GConstruct, GConstructable, gconstruct
  , Representable, gtabulate
  )
import Rel8.Generic.Record ( Record )
import Rel8.Generic.Table.Record ( GColumns )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.Label ( hunlabel )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import qualified Rel8.Schema.Kind as K
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


-- | See 'fanout'. This type makes most sense when @a@ is specialised to
-- something 'Fanoutable'. For example:
--
-- >>> :kind! Fanout i (Name Bool, Name Char, Name Int32)
-- @
-- Fanout i (Name Prelude.Bool, Name Prelude.Char, Name Data.Int.Int32) :: *
-- = Projection i (Name Bool)
--   -> Projection i (Name Char)
--   -> Projection i (Name GHC.Int.Int32)
--   -> Projection i (Name Bool, Name Char, Name GHC.Int.Int32)
-- @
type Fanout :: Type -> Type -> Type
type Fanout i a = GConstruct (TProjection i) (Rep (Record a)) (Projection i a)


-- | The class of 'Table's @a@ with which you can use 'fanout'. In practice,
-- this is any product type whose 'Table' instance is derived generically
-- (including 'Rel8.HKD' and all 'Rel8.Rel8able's).
type Fanoutable :: Type -> Type -> Constraint
class
  ( forall context. Fanoutable' i a context
  , Representable (TProjection i) (Rep (Record a))
  , GColumns TColumns (Rep (Record a)) ~ Columns a
  )
  => Fanoutable i a
instance
  ( forall context. Fanoutable' i a context
  , Representable (TProjection i) (Rep (Record a))
  , GColumns TColumns (Rep (Record a)) ~ Columns a
  )
  => Fanoutable i a


type Fanoutable' :: Type -> Type -> K.HContext -> Constraint
class GConstructable TTop TColumns (TProjection i) context (Rep (Record a))
  => Fanoutable' i a context
instance GConstructable TTop TColumns (TProjection i) context (Rep (Record a))
  => Fanoutable' i a context


lemma :: Fanoutable i a => Dict (Fanoutable' i a) context
lemma = Dict


-- | 'fanout' is a \"smart\" constructor for building 'Table' types in the
-- category of 'Projection's. What 'Control.Category.Cartesian.&&&' is for
-- pairs, 'fanout' is for arbitrary 'Table's. This becomes clearer if we
-- specialise the type a bit:
--
-- >>> :set -XTypeApplications
-- >>> :t fanout @(Name Bool, Name Char, Name Int32)
-- @
-- fanout @(Name Bool, Name Char, Name Int32)
--   :: Projection i (Name Bool)
--     -> Projection i (Name Char)
--     -> Projection i (Name Int32)
--     -> Projection i (Name Bool, Name Char, Int32)
-- @
--
-- When combined with @OverloadedLabels@ for constructing 'Projection's to
-- fields, 'fanout' lets you pretty much build any 'Projection' you could
-- want.
fanout :: forall a i. Fanoutable i a => Fanout i a
fanout =
  gtabulate @(TProjection i) @(Rep (Record a)) @(Projection i a) $ \fields ->
  Projection $ \(i :: Columns i context) -> case lemma @i @a @context of
    Dict ->
      gconstruct
        @TTop
        @TColumns
        @(TProjection i)
        @context
        @(Rep (Record a))
        (\(_ :: proxy x) (Projection f) -> f i)
        fields


data TProjection :: Type -> Type -> Exp Type
type instance Eval (TProjection i a) = Projection i a


data TTop :: Type -> Exp Constraint
type instance Eval (TTop _) = ()
