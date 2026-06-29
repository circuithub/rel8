{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Internal.Column.ADT
  ( HADT
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.Result ( Result )
import Rel8.Internal.Table.ADT ( ADT )


type HADT :: K.Context -> K.Rel8able -> Type
type family HADT context t where
  HADT Result t = t Result
  HADT context t = ADT t context
