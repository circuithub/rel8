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
import Rel8.Kind.Defaulting ( Defaulting( NoDefault ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel )
import Rel8.Schema.HTable.Nullify ( HNullify )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Type.Tag ( EitherTag )


type HEitherTable :: K.HTable -> K.HTable -> K.HTable
data HEitherTable left right context = HEitherTable
  { htag :: HIdentity ('Spec '["isRight"] 'NoDefault EitherTag) context
  , hleft :: HLabel "Left" (HNullify left) context
  , hright :: HLabel "Right" (HNullify right) context
  }
  deriving stock Generic
  deriving anyclass HTable
