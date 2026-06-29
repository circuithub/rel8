{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Rel8.Internal.Expr.Read
  ( read
  , sread
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Expr.Opaleye (unsafeCastExpr, sunsafeCastExpr)
import Rel8.Internal.Schema.Null (Sql)
import Rel8.Internal.Type (DBType)
import Rel8.Internal.Type.Name (TypeName)

-- text
import Data.Text (Text)


read :: Sql DBType a => Expr Text -> Expr a
read = unsafeCastExpr


sread :: TypeName -> Expr Text -> Expr a
sread = sunsafeCastExpr