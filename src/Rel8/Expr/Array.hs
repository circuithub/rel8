{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Rel8.Expr.Array (
  listOf,
  nonEmptyOf,
  slistOf,
  snonEmptyOf,
  sappend,
  sappend1,
  sempty,
)
where

-- base
import Data.List.NonEmpty (NonEmpty)
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (
  fromPrimExpr,
  toPrimExpr,
  zipPrimExprsWith,
 )
import Rel8.Schema.Null (Sql, Unnullify)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Array (array)
import Rel8.Type.Information (TypeInformation (..))


sappend :: Expr [a] -> Expr [a] -> Expr [a]
sappend = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


sappend1 :: Expr (NonEmpty a) -> Expr (NonEmpty a) -> Expr (NonEmpty a)
sappend1 = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


sempty :: TypeInformation (Unnullify a) -> Expr [a]
sempty info = fromPrimExpr $ array info []


slistOf :: TypeInformation (Unnullify a) -> [Expr a] -> Expr [a]
slistOf info = fromPrimExpr . array info . fmap toPrimExpr


snonEmptyOf :: TypeInformation (Unnullify a) -> NonEmpty (Expr a) -> Expr (NonEmpty a)
snonEmptyOf info = fromPrimExpr . array info . fmap toPrimExpr


listOf :: Sql DBType a => [Expr a] -> Expr [a]
listOf = slistOf typeInformation


nonEmptyOf :: Sql DBType a => NonEmpty (Expr a) -> Expr (NonEmpty a)
nonEmptyOf = snonEmptyOf typeInformation
