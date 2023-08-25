{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Rel8.Expr.Read
  ( read
  , sread
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (unsafeCastExpr, sunsafeCastExpr)
import Rel8.Schema.Null (Sql)
import Rel8.Type (DBType)
import Rel8.Type.Name (TypeName)

-- text
import Data.Text (Text)


read :: Sql DBType a => Expr Text -> Expr a
read = unsafeCastExpr


sread :: TypeName -> Expr Text -> Expr a
sread = sunsafeCastExpr