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
import Rel8.Schema.Null ( Unnullify, Nullity( Null, NotNull ), Sql, nullable )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Decoder (Decoder (..))
import Rel8.Type.Information ( TypeInformation(..) )


-- | Produce an expression from a literal.
--
-- Note that you can usually use 'Rel8.lit', but @litExpr@ can solve problems
-- of inference in polymorphic code.
litExpr :: Sql DBType a => a -> Expr a
litExpr = slitExpr nullable typeInformation


slitExpr :: Nullity a -> TypeInformation (Unnullify a) -> a -> Expr a
slitExpr nullity info@TypeInformation {encode} =
  scastExpr info . Expr . encoder
  where
    encoder = case nullity of
      Null -> maybe (Opaleye.ConstExpr Opaleye.NullLit) encode
      NotNull -> encode


sparseValue :: Nullity a -> TypeInformation (Unnullify a) -> Hasql.Row a
sparseValue nullity TypeInformation {decode = Decoder {binary}} = case nullity of
  Null -> Hasql.column $ Hasql.nullable binary
  NotNull -> Hasql.column $ Hasql.nonNullable binary
