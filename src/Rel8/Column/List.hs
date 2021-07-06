{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Column.List
  ( HList
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
import Rel8.Table.List ( ListTable )


-- | Nest a list within a 'Rel8able'. @HList f a@ will produce a 'ListTable'
-- @a@ in the 'Expr' context, and a @[a]@ in the 'Result' context.
type HList :: K.Context -> Type -> Type
type family HList context = list | list -> context where
  HList Aggregate = ListTable Aggregate
  HList Expr = ListTable Expr
  HList Name = ListTable Name
  HList Result = []
