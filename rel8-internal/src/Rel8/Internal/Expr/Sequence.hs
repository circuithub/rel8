module Rel8.Internal.Expr.Sequence
  ( DBSequence
  , nextval
  )
where

-- base
import Data.Int ( Int16, Int32, Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Expr.Opaleye (fromPrimExpr)
import Rel8.Internal.Schema.QualifiedName (QualifiedName, showQualifiedName)

-- | The class of database types that can be sequences and support 'nextval'.
-- 'Int64' is recommended.
-- See: https://www.postgresql.org/docs/current/sql-createsequence.html
class DBSequence a
instance DBSequence Int16
instance DBSequence Int32
instance DBSequence Int64


-- | See https://www.postgresql.org/docs/current/functions-sequence.html
nextval :: DBSequence a => QualifiedName -> Expr a
nextval name =
  fromPrimExpr $
    Opaleye.FunExpr "nextval"
      [ Opaleye.ConstExpr (Opaleye.StringLit (showQualifiedName name))
      ]
