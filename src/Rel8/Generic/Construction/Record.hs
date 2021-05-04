{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Construction.Record
  ( GConstruct, GConstructable, gconstruct, gdeconstruct
  , GFields, Corep, gtabulate, gindex
  , FromColumns, ToColumns
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.Generics
  ( (:*:), K1, M1, U1
  , D, C, S, Meta( MetaSel )
  )
import GHC.TypeLits ( KnownSymbol )
import Prelude

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Table.Record ( GColumns )
import Rel8.Schema.Context.Label ( HLabelable, hlabeler, hunlabeler )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import qualified Rel8.Schema.Kind as K


type FromColumns
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.HContext
  -> Type
type FromColumns _Table _Columns f context = forall proxy x.
  Eval (_Table x) => proxy x -> Eval (_Columns x) context -> Eval (f x)


type ToColumns
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.HContext
  -> Type
type ToColumns _Table _Columns f context = forall proxy x.
  Eval (_Table x) => proxy x -> Eval (f x) -> Eval (_Columns x) context


type GConstruct :: (Type -> Exp Type) -> (Type -> Type) -> Type -> Type
type family GConstruct f rep r where
  GConstruct f (M1 _ _ rep) r = GConstruct f rep r
  GConstruct f (a :*: b) r = GConstruct f a (GConstruct f b r)
  GConstruct _ U1 r = r
  GConstruct f (K1 _ a) r = Eval (f a) -> r


type GFields :: (Type -> Exp Type) -> (Type -> Type) -> Type
type family GFields f rep where
  GFields f (M1 _ _ rep) = GFields f rep
  GFields f (a :*: b) = (GFields f a, GFields f b)
  GFields _ U1 = ()
  GFields f (K1 _ a) = Eval (f a)


type Corep :: (Type -> Exp Type) -> (Type -> Type) -> Constraint
class Corep f rep where
  gtabulate :: (GFields f rep -> a) -> GConstruct f rep a
  gindex :: GConstruct f rep a -> GFields f rep -> a


instance Corep f rep => Corep f (M1 i meta rep) where
  gtabulate = gtabulate @f @rep
  gindex = gindex @f @rep


instance (Corep f a, Corep f b) =>
  Corep f (a :*: b)
 where
  gtabulate f = gtabulate @f @a \a -> gtabulate @f @b \b -> f (a, b)
  gindex f (a, b) = gindex @f @b (gindex @f @a f a) b


instance Corep f U1 where
  gtabulate = ($ ())
  gindex = const


instance Corep f (K1 i a) where
  gtabulate = id
  gindex = id


type GConstructable
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.HContext -> (Type -> Type) -> Constraint
class GConstructable _Table _Columns f context rep where
  gconstruct :: ()
    => ToColumns _Table _Columns f context
    -> GFields f rep
    -> GColumns _Columns rep context
  gdeconstruct :: ()
    => FromColumns _Table _Columns f context
    -> GColumns _Columns rep context
    -> GFields f rep


instance (GConstructable _Table _Columns f context rep) =>
  GConstructable _Table _Columns f context (M1 D meta rep)
 where
  gconstruct = gconstruct @_Table @_Columns @f @context @rep
  gdeconstruct = gdeconstruct @_Table @_Columns @f @context @rep


instance (GConstructable _Table _Columns f context rep) =>
  GConstructable _Table _Columns f context (M1 C meta rep)
 where
  gconstruct = gconstruct @_Table @_Columns @f @context @rep
  gdeconstruct = gdeconstruct @_Table @_Columns @f @context @rep


instance
  ( GConstructable _Table _Columns f context a
  , GConstructable _Table _Columns f context b
  )
  => GConstructable _Table _Columns f context (a :*: b)
 where
  gconstruct toColumns (a, b) = HProduct
    (gconstruct @_Table @_Columns @f @context @a toColumns a)
    (gconstruct @_Table @_Columns @f @context @b toColumns b)
  gdeconstruct fromColumns (HProduct a b) =
    ( gdeconstruct @_Table @_Columns @f @context @a fromColumns a
    , gdeconstruct @_Table @_Columns @f @context @b fromColumns b
    )


instance
  ( Eval (_Table a)
  , HTable (Eval (_Columns a))
  , HLabelable context
  , KnownSymbol label
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  )
  => GConstructable _Table _Columns f context (M1 S meta (K1 i a))
 where
  gconstruct toColumns = hlabel hlabeler . toColumns (Proxy @a)
  gdeconstruct fromColumns = fromColumns (Proxy @a) . hunlabel hunlabeler
