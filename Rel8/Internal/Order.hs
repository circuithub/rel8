{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rel8.Internal.Order where

import Control.Arrow (first)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.Order as O
import qualified Opaleye.Order as O
import qualified Opaleye.PGTypes as O
import Rel8.Internal.Expr
import Rel8.Internal.Operators

--------------------------------------------------------------------------------
data OrderNulls
  = NullsFirst -- ^ @NULLS FIRST@
  | NullsLast  -- ^ @NULLS LAST@
  deriving (Enum,Ord,Eq,Read,Show,Bounded)

-- | Order by a column with the 'ASC' keyword.
asc :: DBOrd b => (a -> Expr b) -> O.Order a
asc f = O.asc (exprToColumn @_ @O.PGInt8 . f)

-- | Order by a column with the 'DESC' keyword.
desc :: DBOrd b => (a -> Expr b) -> O.Order a
desc f = O.desc (exprToColumn @_ @O.PGInt8 . f)

-- | Transform 'asc' or 'desc' to treat nulls specially.
orderNulls
  :: DBOrd b
  => ((a -> Expr b) -> O.Order a) -- ^ 'asc' or 'desc'.
  -> OrderNulls                   -- ^ How @null@ should be ordered.
  -> (a -> Expr (Maybe b))        -- ^ The column to sort on.
  -> O.Order a
orderNulls direction nulls f =
  case direction (unsafeCoerceExpr . f) of
    O.Order g ->
      O.Order
        (\a ->
           map
             (first (\(O.OrderOp orderO _) -> O.OrderOp orderO nullsDir))
             (g a))
  where
    nullsDir =
      case nulls of
        NullsFirst -> O.NullsFirst
        NullsLast -> O.NullsLast
