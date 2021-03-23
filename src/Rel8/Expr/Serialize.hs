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
  ( Nullability( Nullable, NonNullable )
  , Nullabilizes, nullabilization
  )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation(..) )


litExpr :: (DBType db, Nullabilizes db a) => a -> Expr a
litExpr = slitExpr nullabilization typeInformation


slitExpr :: Nullability db a -> TypeInformation db -> a -> Expr a
slitExpr nullable info@TypeInformation {encode} =
  scastExpr nullable info . Expr . encoder
  where
    encoder = case nullable of
      Nullable -> maybe (Opaleye.ConstExpr Opaleye.NullLit) encode
      NonNullable -> encode


sparseValue :: Nullability db a -> TypeInformation db -> Hasql.Row a
sparseValue nullability TypeInformation {decode, out} = case nullability of
  Nullable -> Hasql.column $ Hasql.nullable $ out <$> decode
  NonNullable -> Hasql.column $ Hasql.nonNullable $ out <$> decode
