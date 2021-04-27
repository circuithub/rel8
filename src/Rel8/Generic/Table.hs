{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Table
  ( GTable, GColumns, GContext, GUnreify
  , fromGColumns, toGColumns, gtable, greify, gunreify
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.Generics
  ( (:*:)( (:*:) ), K1( K1 ), M1( M1 )
  , C, D, S
  , Meta( MetaSel )
  )
import GHC.TypeLits ( KnownSymbol )
import Prelude hiding ( null )

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Schema.Context.Label ( HLabelable, hlabeler, hunlabeler )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Product ( HProduct(..) )
import qualified Rel8.Schema.Kind as K


type GColumns :: (Type -> Exp K.HTable) -> (Type -> Type) -> K.HTable
type family GColumns _Columns rep where
  GColumns _Columns (M1 D _ rep) = GColumns _Columns rep
  GColumns _Columns (M1 C _ rep) = GColumns _Columns rep
  GColumns _Columns (rep1 :*: rep2) =
    HProduct (GColumns _Columns rep1) (GColumns _Columns rep2)
  GColumns _Columns (M1 S ('MetaSel ('Just label) _ _ _) (K1 _ a)) =
    HLabel label (Eval (_Columns a))


type GContext :: (Type -> Exp a) -> (Type -> Type) -> a
type family GContext _Context rep where
  GContext _Context (M1 _ _ rep) = GContext _Context rep
  GContext _Context (rep1 :*: _rep2) = GContext _Context rep1
  GContext _Context (K1 _ a) = Eval (_Context a)


type GUnreify :: (Type -> Exp a) -> (Type -> Type) -> Type -> Type
type family GUnreify _Unreify rep where
  GUnreify _Unreify (M1 i c rep) = M1 i c (GUnreify _Unreify rep)
  GUnreify _Unreify (rep1 :*: rep2) =
    GUnreify _Unreify rep1 :*: GUnreify _Unreify rep2
  GUnreify _Unreify (K1 i a) = K1 i (Eval (_Unreify a))


type GTable
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> K.HContext -> (Type -> Type) -> Constraint
class GTable _Table _Columns context rep
 where
  fromGColumns :: ()
    => (forall a. Eval (_Table a) => Eval (_Columns a) context -> a)
    -> GColumns _Columns rep context
    -> rep x

  toGColumns :: ()
    => (forall a. Eval (_Table a) => a -> Eval (_Columns a) context)
    -> rep x
    -> GColumns _Columns rep context

  gtable :: ()
    => (forall a. Eval (_Table a) => Proxy a -> Eval (_Columns a) context)
    -> GColumns _Columns rep context

  greify :: ()
    => Proxy _Unreify
    -> (forall a. Eval (_Table a) => Eval (_Unreify a) -> a)
    -> GUnreify _Unreify rep x
    -> rep x

  gunreify :: ()
    => Proxy _Unreify
    -> (forall a. Eval (_Table a) => a -> Eval (_Unreify a))
    -> rep x
    -> GUnreify _Unreify rep x


instance GTable _Table _Columns context rep =>
  GTable _Table _Columns context (M1 D c rep)
 where
  fromGColumns fromColumns =
    M1 . fromGColumns @_Table @_Columns @context @rep fromColumns
  toGColumns toColumns (M1 a) =
    toGColumns @_Table @_Columns @context @rep toColumns a
  gtable = gtable @_Table @_Columns @context @rep
  greify proxy reify (M1 a) =
    M1 (greify @_Table @_Columns @context @rep proxy reify a)
  gunreify proxy unreify (M1 a) =
    M1 (gunreify @_Table @_Columns @context @rep proxy unreify a)


instance GTable _Table _Columns context rep =>
  GTable _Table _Columns context (M1 C c rep)
 where
  fromGColumns fromColumns =
    M1 . fromGColumns @_Table @_Columns @context @rep fromColumns
  toGColumns toColumns (M1 a) =
    toGColumns @_Table @_Columns @context @rep toColumns a
  gtable = gtable @_Table @_Columns @context @rep
  greify proxy reify (M1 a) =
    M1 (greify @_Table @_Columns @context @rep proxy reify a)
  gunreify proxy unreify (M1 a) =
    M1 (gunreify @_Table @_Columns @context @rep proxy unreify a)


instance
  ( GTable _Table _Columns context rep1
  , GTable _Table _Columns context rep2
  )
  => GTable _Table _Columns context (rep1 :*: rep2)
 where
  fromGColumns fromColumns (HProduct a b) =
    fromGColumns @_Table @_Columns @context @rep1 fromColumns a :*:
    fromGColumns @_Table @_Columns @context @rep2 fromColumns b
  toGColumns toColumns (a :*: b) = HProduct
    (toGColumns @_Table @_Columns @context @rep1 toColumns a)
    (toGColumns @_Table @_Columns @context @rep2 toColumns b)
  gtable table = HProduct
    (gtable @_Table @_Columns @context @rep1 table)
    (gtable @_Table @_Columns @context @rep2 table)
  greify proxy reify (a :*: b) =
    greify @_Table @_Columns @context @rep1 proxy reify a :*:
    greify @_Table @_Columns @context @rep2 proxy reify b
  gunreify proxy unreify (a :*: b) =
    gunreify @_Table @_Columns @context @rep1 proxy unreify a :*:
    gunreify @_Table @_Columns @context @rep2 proxy unreify b


instance
  ( HTable (Eval (_Columns a))
  , Eval (_Table a)
  , HLabelable context
  , KnownSymbol label
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , k1 ~ K1 i a
  )
  => GTable _Table _Columns context (M1 S meta k1)
 where
  fromGColumns fromColumns = M1 . K1 . fromColumns . hunlabel hunlabeler
  toGColumns toColumns (M1 (K1 a)) = hlabel hlabeler (toColumns a)
  gtable table = hlabel hlabeler (table (Proxy @a))
  greify _ reify (M1 (K1 a)) = M1 (K1 (reify a))
  gunreify _ unreify (M1 (K1 a)) = M1 (K1 (unreify a))
