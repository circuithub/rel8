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
  ( GTable, GColumns, GContext, fromGColumns, toGColumns, gtable
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


instance GTable _Table _Columns context rep =>
  GTable _Table _Columns context (M1 D c rep)
 where
  fromGColumns fromColumns =
    M1 . fromGColumns @_Table @_Columns @context @rep fromColumns
  toGColumns toColumns (M1 a) =
    toGColumns @_Table @_Columns @context @rep toColumns a
  gtable = gtable @_Table @_Columns @context @rep


instance GTable _Table _Columns context rep =>
  GTable _Table _Columns context (M1 C c rep)
 where
  fromGColumns fromColumns =
    M1 . fromGColumns @_Table @_Columns @context @rep fromColumns
  toGColumns toColumns (M1 a) =
    toGColumns @_Table @_Columns @context @rep toColumns a
  gtable = gtable @_Table @_Columns @context @rep


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


instance
  ( HTable (Eval (_Columns a))
  , Eval (_Table a)
  , HLabelable context
  , KnownSymbol label
  , GColumns _Columns (M1 S meta k1) ~ HLabel label (Eval (_Columns a))
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , k1 ~ K1 i a
  )
  => GTable _Table _Columns context (M1 S meta k1)
 where
  fromGColumns fromColumns = M1 . K1 . fromColumns . hunlabel hunlabeler
  toGColumns toColumns (M1 (K1 a)) = hlabel hlabeler (toColumns a)
  gtable table = hlabel hlabeler (table (Proxy @a))
