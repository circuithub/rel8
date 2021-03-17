{-# language NamedFieldPuns #-}
{-# language PartialTypeSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Expr.Lit ( litExpr, litExprWith, unsafeLiteral ) where

import {-# source #-} Rel8.Expr ( Expr( Expr ) )

-- opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.DBType ( DBType, typeInformation )
import Rel8.DatabaseType ( DatabaseType( DatabaseType, encode, typeName ) )
import Rel8.Expr.Opaleye ( columnToExpr )


-- | Construct a SQL expression from some literal text. The provided literal
-- will be interpolated exactly as specified with no escaping.
unsafeLiteral :: forall a. String -> Expr a
unsafeLiteral = columnToExpr @a @a . Opaleye.Column . Opaleye.ConstExpr . Opaleye.OtherLit


litExpr :: DBType a => a -> Expr a
litExpr = litExprWith typeInformation


litExprWith :: DatabaseType a -> a -> Expr a
litExprWith DatabaseType{ encode, typeName } = Expr . Opaleye.CastExpr typeName . encode
