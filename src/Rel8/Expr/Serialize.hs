{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.Expr.Serialize
  ( litExpr
  , slitExpr
  , sparseValue
  )
where

-- base
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye ( scastExpr )
import Rel8.Schema.Nullability
  ( Unnullify
  , Nullability( Nullable, NonNullable )
  , Sql, nullabilization
  )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation(..) )


-- | Produce an expression from a literal.
--
-- Note that you can usually use 'lit', but @litExpr@ can solve problems of
-- inference in polymorphic code.
litExpr :: Sql DBType a => a -> Expr a
litExpr = slitExpr nullabilization typeInformation


slitExpr :: Nullability a -> TypeInformation (Unnullify a) -> a -> Expr a
slitExpr nullability info@TypeInformation {encode} =
  scastExpr info . Expr . encoder
  where
    encoder = case nullability of
      Nullable -> maybe (Opaleye.ConstExpr Opaleye.NullLit) encode
      NonNullable -> encode


sparseValue :: Nullability a -> TypeInformation (Unnullify a) -> Hasql.Row a
sparseValue nullability TypeInformation {decode} = case nullability of
  Nullable -> Hasql.column $ Hasql.nullable decode
  NonNullable -> Hasql.column $ Hasql.nonNullable decode
