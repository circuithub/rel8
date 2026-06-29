module Rel8.Internal.Expr.Show
  ( show
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Expr.Opaleye (unsafeCastExpr)

-- text
import Data.Text (Text)


show :: Expr a -> Expr Text
show = unsafeCastExpr