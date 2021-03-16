{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Either
  ( HEitherTable(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude ()

-- rel8
import Rel8.Kind.Blueprint ( Blueprint( Scalar ) )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Nullify ( HNullify )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Type.Tag ( EitherTag )


type HEitherTable :: HKTable -> HKTable -> HKTable
data HEitherTable left right context = HEitherTable
  { htag :: HIdentity ('Spec '["isRight"] 'Required 'NonNullable ('Scalar EitherTag)) context
  , hleft :: HNullify left context
  , hright :: HNullify right context
  }
  deriving stock Generic
  deriving anyclass HTable
