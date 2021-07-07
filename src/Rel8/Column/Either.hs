{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Column.Either
  ( HEither
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result ( Result )
import Rel8.Table.Either ( EitherTable )


-- | Nest an 'Either' value within a 'Rel8able'. @HEither f a b@ will produce a
-- 'EitherTable' @a b@ in the 'Expr' context, and a 'Either' @a b@ in the
-- 'Result' context.
type HEither :: K.Context -> Type -> Type -> Type
type family HEither context = either | either -> context where
  HEither Result = Either
  HEither context = EitherTable context
