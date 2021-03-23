{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Array
  ( listOf, nonEmptyOf
  , sappend, sappend1, sempty
  )
where

-- base
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye
  ( fromPrimExpr, toPrimExpr
  , zipPrimExprsWith
  )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Array ( array, zipPrimArraysWith )
import Rel8.Type.Information ( TypeInformation(..) )
import Rel8.Schema.Nullability ( Nullability, Nullabilizes )


sappend :: Expr [a] -> Expr [a] -> Expr [a]
sappend = zipPrimExprsWith (zipPrimArraysWith (Opaleye.BinExpr (Opaleye.:||)))


sappend1 :: Expr (NonEmpty a) -> Expr (NonEmpty a) -> Expr (NonEmpty a)
sappend1 = zipPrimExprsWith (zipPrimArraysWith (Opaleye.BinExpr (Opaleye.:||)))


sempty :: Nullability t a -> TypeInformation t -> Expr [a]
sempty _ info = fromPrimExpr $ array info []


listOf :: forall a t. (Nullabilizes t a, DBType t)
  => [Expr a] -> Expr [a]
listOf = fromPrimExpr . array (typeInformation @t) . fmap toPrimExpr


nonEmptyOf :: forall a t. (Nullabilizes t a, DBType t)
  => NonEmpty (Expr a) -> Expr (NonEmpty a)
nonEmptyOf = fromPrimExpr . array (typeInformation @t) . fmap toPrimExpr
