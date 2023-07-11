module Rel8.Expr.Sequence
  ( nextval
  )
where

-- base
import Data.Int ( Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye (fromPrimExpr)
import Rel8.Schema.QualifiedName (QualifiedName, ppQualifiedName)


-- | See https://www.postgresql.org/docs/current/functions-sequence.html
nextval :: QualifiedName -> Expr Int64
nextval name =
  fromPrimExpr $
    Opaleye.FunExpr "nextval"
      [ Opaleye.ConstExpr (Opaleye.StringLit (show (ppQualifiedName name)))
      ]
