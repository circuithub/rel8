{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Column.These
  ( HThese
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )
import Rel8.Table.These ( TheseTable )

-- these
import Data.These ( These )


-- | Nest an 'These' value within a 'Rel8able'. @HThese f a b@ will produce a
-- 'TheseTable' @a b@ in the 'Expr' context, and a 'These' @a b@ in the
-- 'Result' context.
type HThese :: K.Context -> Type -> Type -> Type
type family HThese context = these | these -> context where
  HThese Aggregate = TheseTable Aggregate
  HThese Expr = TheseTable Expr
  HThese Name = TheseTable Name
  HThese Result = These
