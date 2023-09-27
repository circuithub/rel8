module Rel8.Expr.Show
  ( show
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (unsafeCastExpr)

-- text
import Data.Text (Text)


show :: Expr a -> Expr Text
show = unsafeCastExpr