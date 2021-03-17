{-# language NamedFieldPuns #-}
{-# language PartialTypeSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Expr.Lit ( litExpr, litExprWith ) where

import {-# source #-} Rel8.Expr ( Expr( Expr ) )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DBType ( DBType, typeInformation )
import Rel8.DatabaseType ( DatabaseType( DatabaseType, encode, typeName ) )


litExpr :: DBType a => a -> Expr a
litExpr = litExprWith typeInformation


litExprWith :: DatabaseType a -> a -> Expr a
litExprWith DatabaseType{ encode, typeName } = Expr . Opaleye.CastExpr typeName . encode
