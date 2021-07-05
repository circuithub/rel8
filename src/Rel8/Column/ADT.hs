{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Column.ADT
  ( HADT
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result ( Result )
import Rel8.Table.ADT ( ADT )


type HADT :: K.Context -> K.Rel8able -> Type
type family HADT context t where
  HADT Result t = t Result
  HADT context t = ADT t context
