{-# language FlexibleInstances #-}
{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

module Rel8.Table.Semigroup
  ( SemigroupTable, (<>:)
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( HConstrainTable, htabulate, hfield, hdicts )
import Rel8.Schema.Spec.ConstrainContext ( ConstrainContext )
import Rel8.Table ( Table, Context, Columns, fromColumns, toColumns )


type SemigroupTable :: Type -> Constraint
class
  ( Table a
  , HConstrainTable (Columns a) (ConstrainContext Semigroup (Context a))
  ) => SemigroupTable a
instance
  ( Table a
  , HConstrainTable (Columns a) (ConstrainContext Semigroup (Context a))
  ) => SemigroupTable a


(<>:) :: forall a. SemigroupTable a => a -> a -> a
(toColumns -> as) <>: (toColumns -> bs) = fromColumns $
  htabulate $ \field -> case hfield dicts field of
    Dict -> hfield as field <> hfield bs field
  where
    dicts = hdicts @(Columns a) @(ConstrainContext Semigroup (Context a))
infixr 6 <>:
