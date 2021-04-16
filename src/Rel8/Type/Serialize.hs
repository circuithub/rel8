{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Type.Serialize
  ( Strategy( ExprsFor, encode, decode )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.Result ( Col( R ), Result )
import Rel8.Schema.Serialize ( Constraints )
import Rel8.Table ( Columns )


type Strategy :: Type -> Constraint
class Strategy strategy where
  type ExprsFor strategy (a :: Type) = (expr :: Type) | expr -> a strategy

  encode :: forall a expr result.
    ( expr ~ ExprsFor strategy a
    , result ~ Columns expr (Col Result)
    , Constraints strategy a
    )
    => a -> result

  decode :: forall a expr result.
    ( expr ~ ExprsFor strategy a
    , result ~ Columns expr (Col Result)
    , Constraints strategy a
    )
    => result -> a


instance Strategy (Proxy Expr) where
  type ExprsFor (Proxy Expr) a = Expr a

  decode (HType (R a)) = a
  encode = HType . R
