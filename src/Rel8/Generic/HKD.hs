{-# language DataKinds #-}
{-# language FlexibleContexts #-} 
{-# language FlexibleInstances #-} 
{-# language FunctionalDependencies #-} 
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

{-# options -Wno-orphans #-}

module Rel8.Generic.HKD
  ( GTable, GColumns, gfromColumns, gtoColumns
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics
  ( (:*:)( (:*:) ), K1( K1 ), M1( M1 ), C, D, S, Meta( MetaSel )
  , Rep
  )
import GHC.TypeLits ( KnownSymbol )
import Prelude

-- higgledy
import Data.Generic.HKD ( HKD(..), GHKD_ )

-- rel8
import Rel8.Aggregate ( Col(..) )
import Rel8.Schema.Context.Label
  ( Labelable
  , HLabelable, hlabeler, hunlabeler
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import Rel8.Schema.HTable.Type ( HType )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( NotReify, notReify )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


type Column1Helper :: K.Context -> (Type -> Type) -> Type -> Constraint
class
  ( Table context (f a)
  , Context1 f ~ context
  , Context (f a) ~ context
  , Columns (f a) ~ HType a
  )
  => Column1Helper context f a | f -> context
instance
  ( Table context (f a)
  , Context1 f ~ context
  , Context (f a) ~ context
  , Columns (f a) ~ HType a
  )
  => Column1Helper context f a


type Column1 :: K.Context -> (Type -> Type) -> Constraint
class
  ( forall a. Sql DBType a => Column1Helper context f a
  , Context1 f ~ context
  )
  => Column1 context f | f -> context
instance
  ( forall a. Sql DBType a => Column1Helper context f a
  , Context1 f ~ context
  )
  => Column1 context f


type Context1 :: (Type -> Type) -> K.Context
type Context1 f = Context (f Bool)


toColumn1 :: forall a f context. (Column1 context f, Sql DBType a)
  => f a -> HType a (Col context)
toColumn1 = case Dict @(Column1Helper context f) @a of
  Dict -> toColumns


fromColumn1 :: forall a f context. (Column1 context f, Sql DBType a)
  => HType a (Col context) -> f a
fromColumn1 = case Dict @(Column1Helper context f) @a of
  Dict -> fromColumns


type Recontextualize1
  :: K.Context
  -> K.Context
  -> (Type -> Type)
  -> (Type -> Type)
  -> Constraint
class Recontextualize context context' (f Bool) (f' Bool) =>
  Recontextualize1 context context' f f'
instance Recontextualize context context' (f Bool) (f' Bool) =>
  Recontextualize1 context context' f f'


class HTable (GColumns rep) => GTable rep where
  gtoColumns :: HLabelable context
    => (forall a. Sql DBType a => f a -> HType a context)
    -> GHKD_ f rep x
    -> GColumns rep context
  gfromColumns :: HLabelable context
    => (forall a. Sql DBType a => HType a context -> f a)
    -> GColumns rep context
    -> GHKD_ f rep x


instance GTable rep => GTable (M1 D c rep) where
  gtoColumns f (M1 a) = gtoColumns f a
  {-# INLINABLE gtoColumns #-}

  gfromColumns f = M1 . gfromColumns f
  {-# INLINABLE gfromColumns #-}


instance GTable rep => GTable (M1 C c rep) where
  gtoColumns f (M1 a) = gtoColumns f a
  {-# INLINABLE gtoColumns #-}

  gfromColumns f = M1 . gfromColumns f
  {-# INLINABLE gfromColumns #-}


instance (KnownSymbol name, Sql DBType a) =>
  GTable (M1 S ('MetaSel ('Just name) _su _ss _ds) (K1 i a))
 where
  gtoColumns f (M1 (K1 a)) = hlabel hlabeler (f a)
  {-# INLINABLE gtoColumns #-}

  gfromColumns f a = M1 (K1 (f (hunlabel hunlabeler a)))
  {-# INLINABLE gfromColumns #-}


instance (GTable f, GTable g) => GTable (f :*: g) where
  gtoColumns f (x :*: y) = HProduct (gtoColumns f x) (gtoColumns f y)
  {-# INLINABLE gtoColumns #-}

  gfromColumns f (HProduct x y) = gfromColumns f x :*: gfromColumns f y
  {-# INLINABLE gfromColumns #-}


type GColumns :: (Type -> Type) -> K.HTable
type family GColumns rep where
  GColumns (M1 D _ f) = GColumns f
  GColumns (M1 C _ f) = GColumns f
  GColumns (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ a)) =
    HLabel name (HType a)
  GColumns (f :*: g) = HProduct (GColumns f) (GColumns g)


instance
  ( GTable (Rep a)
  , Column1 context f
  , Labelable context
  , NotReify context
  )
  => Table context (HKD a f)
 where
  type Columns (HKD a f) = GColumns (Rep a)
  type Context (HKD a f) = Context1 f

  toColumns = gtoColumns toColumn1 . runHKD
  fromColumns = HKD . gfromColumns fromColumn1
  reify = notReify
  unreify = notReify


instance
  ( a ~ a'
  , GTable (Rep a)
  , NotReify context, NotReify context'
  , Recontextualize1 context context' f f'
  , Column1 context f, Labelable context
  , Column1 context' f', Labelable context'
  )
  => Recontextualize
    context
    context'
    (HKD a f)
    (HKD a' f')
