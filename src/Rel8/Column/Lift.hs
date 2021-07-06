{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Column.Lift
  ( Lift
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result ( Result )
import Rel8.Table.HKD ( HKD )


type Lift :: K.Context -> Type -> Type
type family Lift context a where
  Lift Result a = a
  Lift context a = HKD a context
