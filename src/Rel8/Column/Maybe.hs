{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Column.Maybe
  ( HMaybe
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )
import Rel8.Table.Maybe ( MaybeTable )


-- | Nest a 'Maybe' value within a 'Rel8able'. @HMaybe f a@ will produce a
-- 'MaybeTable' @a@ in the 'Expr' context, and a 'Maybe' @a@ in the 'Result'
-- context.
type HMaybe :: K.Context -> Type -> Type
type family HMaybe context = maybe | maybe -> context where
  HMaybe Aggregate = MaybeTable Aggregate
  HMaybe Expr = MaybeTable Expr
  HMaybe Name = MaybeTable Name
  HMaybe Result = Maybe
