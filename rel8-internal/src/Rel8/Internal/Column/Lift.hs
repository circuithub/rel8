{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Internal.Column.Lift
  ( Lift
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.Result ( Result )
import Rel8.Internal.Table.HKD ( HKD )


type Lift :: K.Context -> Type -> Type
type family Lift context a where
  Lift Result a = a
  Lift context a = HKD a context
