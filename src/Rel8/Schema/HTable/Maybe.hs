{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.Schema.HTable.Maybe
  ( HMaybeTable(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude ()

-- rel8
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Kind.Nullability ( Nullability( Nullable ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Functor ( HFunctor, hmap )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Nullify ( HNullify )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Type.Tag ( MaybeTag )


data HMaybeTable table context = HMaybeTable
  { htag :: HIdentity ('Spec 'Required 'Nullable MaybeTag) context
  , htable :: HNullify table context
  }
  deriving stock Generic
  deriving anyclass HTable


instance HFunctor HMaybeTable where
  hmap f HMaybeTable {htag, htable} = HMaybeTable
    { htag
    , htable = hmap f htable
    }
