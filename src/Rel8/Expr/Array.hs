{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Expr.Array
  ( listOf, nonEmptyOf
  , sappend, sempty
  )
where

-- base
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye ( unsafeFromPrimExpr, zipPrimExprsWith )
import Rel8.Kind.Emptiability
  ( Emptiability( Emptiable, NonEmptiable )
  )
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Array ( Array, array )
import Rel8.Type.Information ( TypeInformation(..) )


sappend :: ()
  => Expr nullability' (Array emptiability nullability a)
  -> Expr nullability' (Array emptiability nullability a)
  -> Expr nullability' (Array emptiability nullability a)
sappend = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


sempty :: ()
  => TypeInformation a -> Expr nullability' (Array 'Emptiable nullability a)
sempty info = unsafeFromPrimExpr $ array info []


listOf :: forall a nullability. DBType a
  => [Expr nullability a]
  -> Expr 'NonNullable (Array 'Emptiable nullability a)
listOf =
  unsafeFromPrimExpr . array (typeInformation @a) . fmap (\(Expr a) -> a)


nonEmptyOf :: forall a nullability. DBType a
  => NonEmpty (Expr nullability a)
  -> Expr 'NonNullable (Array 'NonEmptiable nullability a)
nonEmptyOf =
  unsafeFromPrimExpr . array (typeInformation @a) . fmap (\(Expr a) -> a)
