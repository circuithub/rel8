{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Internal.Column.Maybe
  ( HMaybe
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.Result ( Result )
import Rel8.Internal.Table.Maybe ( MaybeTable )


-- | Nest a 'Maybe' value within a 'Rel8able'. @HMaybe f a@ will produce a
-- 'MaybeTable' @a@ in the 'Expr' context, and a 'Maybe' @a@ in the 'Result'
-- context.
type HMaybe :: K.Context -> Type -> Type
type family HMaybe context = maybe | maybe -> context where
  HMaybe Result = Maybe
  HMaybe context = MaybeTable context
