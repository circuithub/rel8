{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
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

module Rel8.Generic.Table.Record
  ( GTable, GColumns, GContext, gfromColumns, gtoColumns, gtable
  , GSerialize, gfromResult, gtoResult
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
import Prelude hiding ( null )

-- rel8
import Rel8.FCF ( Eval, Exp )
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


type GContext :: (Type -> Exp K.Context) -> (Type -> Type) -> K.Context
type family GContext _Context rep where
  GContext _Context (M1 _ _ rep) = GContext _Context rep
  GContext _Context (rep1 :*: _rep2) = GContext _Context rep1
  GContext _Context (K1 _ a) = Eval (_Context a)


type GTable
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Type) -> Constraint
class GTable _Table _Columns rep
 where
  gfromColumns :: ()
    => (forall a. Eval (_Table a) => Eval (_Columns a) context -> a)
    -> GColumns _Columns rep context
    -> rep x

  gtoColumns :: ()
    => (forall a. Eval (_Table a) => a -> Eval (_Columns a) context)
    -> rep x
    -> GColumns _Columns rep context

  gtable :: ()
    => (forall a proxy. Eval (_Table a) => proxy a -> Eval (_Columns a) context)
    -> GColumns _Columns rep context


type GSerialize
  :: (Type -> Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Type) -> (Type -> Type) -> Constraint
class GSerialize _Serialize _Columns exprs rep
 where
  gfromResult :: ()
    => (forall expr a proxy. Eval (_Serialize expr a)
        => proxy expr -> Eval (_Columns expr) context -> a)
    -> GColumns _Columns exprs context
    -> rep x

  gtoResult :: ()
    => (forall expr a proxy. Eval (_Serialize expr a)
        => proxy expr -> a -> Eval (_Columns expr) context)
    -> rep x
    -> GColumns _Columns exprs context


instance GTable _Table _Columns rep => GTable _Table _Columns (M1 D c rep)
 where
  gfromColumns fromColumns =
    M1 . gfromColumns @_Table @_Columns @rep fromColumns
  gtoColumns toColumns (M1 a) =
    gtoColumns @_Table @_Columns @rep toColumns a
  gtable = gtable @_Table @_Columns @rep


instance GSerialize _Serialize _Columns exprs rep =>
  GSerialize _Serialize _Columns (M1 D c exprs) (M1 D c rep)
 where
  gfromResult fromResult =
    M1 . gfromResult @_Serialize @_Columns @exprs @rep fromResult
  gtoResult toResult (M1 a) =
    gtoResult @_Serialize @_Columns @exprs @rep toResult a


instance GTable _Table _Columns rep => GTable _Table _Columns (M1 C c rep)
 where
  gfromColumns fromColumns =
    M1 . gfromColumns @_Table @_Columns @rep fromColumns
  gtoColumns toColumns (M1 a) =
    gtoColumns @_Table @_Columns @rep toColumns a
  gtable = gtable @_Table @_Columns @rep


instance GSerialize _Serialize _Columns exprs rep =>
  GSerialize _Serialize _Columns (M1 C c exprs) (M1 C c rep)
 where
  gfromResult fromResult =
    M1 . gfromResult @_Serialize @_Columns @exprs @rep fromResult
  gtoResult toResult (M1 a) =
    gtoResult @_Serialize @_Columns @exprs @rep toResult a


instance (GTable _Table _Columns rep1, GTable _Table _Columns rep2) =>
  GTable _Table _Columns (rep1 :*: rep2)
 where
  gfromColumns fromColumns (HProduct a b) =
    gfromColumns @_Table @_Columns @rep1 fromColumns a :*:
    gfromColumns @_Table @_Columns @rep2 fromColumns b
  gtoColumns toColumns (a :*: b) = HProduct
    (gtoColumns @_Table @_Columns @rep1 toColumns a)
    (gtoColumns @_Table @_Columns @rep2 toColumns b)
  gtable table = HProduct
    (gtable @_Table @_Columns @rep1 table)
    (gtable @_Table @_Columns @rep2 table)


instance
  ( GSerialize _Serialize _Columns expr1 rep1
  , GSerialize _Serialize _Columns expr2 rep2
  )
  => GSerialize _Serialize _Columns (expr1 :*: expr2) (rep1 :*: rep2)
 where
  gfromResult fromResult (HProduct a b) =
    gfromResult @_Serialize @_Columns @expr1 @rep1 fromResult a :*:
    gfromResult @_Serialize @_Columns @expr2 @rep2 fromResult b
  gtoResult toResult (a :*: b) =
    HProduct
      (gtoResult @_Serialize @_Columns @expr1 @rep1 toResult a)
      (gtoResult @_Serialize @_Columns @expr2 @rep2 toResult b)


instance
  ( Eval (_Table a)
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , k1 ~ K1 i a
  )
  => GTable _Table _Columns (M1 S meta k1)
 where
  gfromColumns fromColumns = M1 . K1 . fromColumns . hunlabel
  gtoColumns toColumns (M1 (K1 a)) = hlabel (toColumns a)
  gtable table = hlabel (table (Proxy @a))


instance
  ( Eval (_Serialize expr a)
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , k1 ~ K1 i expr
  , k1' ~ K1 i a
  )
  => GSerialize _Serialize _Columns (M1 S meta k1) (M1 S meta k1')
 where
  gfromResult fromResult = M1 . K1 . fromResult (Proxy @expr) . hunlabel
  gtoResult toResult (M1 (K1 a)) = hlabel (toResult (Proxy @expr) a)
