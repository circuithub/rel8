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

module Rel8.Generic.Coherence
  ( GCoherent
  , gcoherence
  , gcongruence
  )
where

-- base
import Control.Category ( id )
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Type.Equality ( (:~:), apply )
import GHC.Generics ( (:*:), K1, M1, Meta( MetaSel ), C, D, S )
import Prelude hiding ( id )

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Map ( GMap )
import Rel8.Generic.Table.Record ( GColumns, GContext )


type GCoherent :: (Type -> Exp Constraint) -> (Type -> Type) -> Constraint
class GCoherent _Table rep where
  gcoherence :: ()
    => proxy _Context
    -> proxy' f
    -> (forall a proxy_. Eval (_Table a)
          => proxy_ a -> Eval (_Context (Eval (f a))) :~: context)
    -> GContext _Context (GMap f rep) :~: context

  gcongruence :: ()
    => proxy _Columns
    -> proxy' f
    -> (forall a proxy_. Eval (_Table a)
          => proxy_ a -> Eval (_Columns a) :~: Eval (_Columns (Eval (f a))))
    -> GColumns _Columns rep :~: GColumns _Columns (GMap f rep)


instance GCoherent _Table rep => GCoherent _Table (M1 D c rep) where
  gcoherence = gcoherence @_Table @rep
  gcongruence = gcongruence @_Table @rep


instance GCoherent _Table rep => GCoherent _Table (M1 C c rep) where
  gcoherence = gcoherence @_Table @rep
  gcongruence = gcongruence @_Table @rep


instance (GCoherent _Table rep1, GCoherent _Table rep2) =>
  GCoherent _Table (rep1 :*: rep2)
 where
  gcoherence = gcoherence @_Table @rep1
  gcongruence _Columns f proof = id `apply`
    gcongruence @_Table @rep1 _Columns f proof `apply`
    gcongruence @_Table @rep2 _Columns f proof


instance
  ( Eval (_Table a)
  , meta ~ 'MetaSel ('Just _l) _su _ss _ds
  , k1 ~ K1 i a
  )
  => GCoherent _Table (M1 S meta k1)
 where
  gcoherence _ _ proof = proof (Proxy @a)
  gcongruence _ _ proof = id `apply` proof (Proxy @a)
