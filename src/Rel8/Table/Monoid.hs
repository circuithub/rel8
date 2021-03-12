{-# language FlexibleInstances #-}
{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.Monoid
  ( MonoidTable, memptyTable
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( HConstrainTable, htabulate, hfield, hdicts )
import Rel8.Schema.Spec.ConstrainContext ( ConstrainContext )
import Rel8.Table ( Columns, Context, fromColumns )
import Rel8.Table.Semigroup ( SemigroupTable )


type MonoidTable :: Type -> Constraint
class
  ( SemigroupTable a
  , HConstrainTable (Columns a) (ConstrainContext Monoid (Context a))
  ) => MonoidTable a
instance
  ( SemigroupTable a
  , HConstrainTable (Columns a) (ConstrainContext Monoid (Context a))
  ) => MonoidTable a


memptyTable :: forall a. MonoidTable a => a
memptyTable = fromColumns $ htabulate $ \field -> case hfield dicts field of
  Dict -> mempty
  where
    dicts = hdicts @(Columns a) @(ConstrainContext Monoid (Context a))
