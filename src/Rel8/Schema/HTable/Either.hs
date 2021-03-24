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
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Nullify ( HNullify )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Type.Tag ( EitherTag )


type HEitherTable :: K.HTable -> K.HTable -> K.HTable
data HEitherTable left right context = HEitherTable
  { htag :: HIdentity ('Spec '["isRight"] 'Required EitherTag EitherTag) context
  , hleft :: HNullify left context
  , hright :: HNullify right context
  }
  deriving stock Generic
  deriving anyclass HTable
