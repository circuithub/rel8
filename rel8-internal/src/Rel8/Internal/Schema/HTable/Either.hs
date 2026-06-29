{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Internal.Schema.HTable.Either
  ( HEitherTable(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude ()

-- rel8
import Rel8.Internal.Schema.HTable ( HTable )
import Rel8.Internal.Schema.HTable.Identity ( HIdentity )
import Rel8.Internal.Schema.HTable.Label ( HLabel )
import Rel8.Internal.Schema.HTable.Nullify ( HNullify )
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Type.Tag ( EitherTag )


type HEitherTable :: K.HTable -> K.HTable -> K.HTable
data HEitherTable left right context = HEitherTable
  { htag :: HLabel "isRight" (HIdentity EitherTag) context
  , hleft :: HLabel "Left" (HNullify left) context
  , hright :: HLabel "Right" (HNullify right) context
  }
  deriving stock Generic
  deriving anyclass HTable
