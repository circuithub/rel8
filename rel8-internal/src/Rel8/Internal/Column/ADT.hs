{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Internal.Column.ADT
  ( HADT
  , HADTWith
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.Result ( Result )
import Rel8.Internal.Table.ADT ( ADTWith )
import Rel8.Internal.Type.Tag ( Tag )


type HADT :: K.Context -> K.Rel8able -> Type
type HADT context t = HADTWith Tag context t


-- | 'HADT' for an 'ADTWith' whose tag column has type @tag@.
type HADTWith :: Type -> K.Context -> K.Rel8able -> Type
type family HADTWith tag context t where
  HADTWith _tag Result t = t Result
  HADTWith tag context t = ADTWith tag t context
