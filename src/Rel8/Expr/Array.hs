{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Expr.Array
  ( listOf, nonEmptyOf
  )
where

-- base
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Kind.Emptiability ( Emptiability( Emptiable, NonEmptiable ) )
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Array ( Array, array )


listOf :: forall a nullability. DBType a
  => [Expr nullability a]
  -> Expr 'NonNullable (Array 'Emptiable nullability a)
listOf = Expr . array (typeInformation @a) . fmap (\(Expr a) -> a)


nonEmptyOf :: forall a nullability. DBType a
  => NonEmpty (Expr nullability a)
  -> Expr 'NonNullable (Array 'NonEmptiable nullability a)
nonEmptyOf = Expr . array (typeInformation @a) . fmap (\(Expr a) -> a)
