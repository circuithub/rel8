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
  = NullsFirst
  | NullsLast
  deriving (Enum,Ord,Eq,Read,Show,Bounded)

asc :: forall a b. DBOrd b => (a -> Expr b) -> O.Order a
asc f = O.asc (exprToColumn @b @O.PGInt8 . f)

desc :: forall a b. DBOrd b => (a -> Expr b) -> O.Order a
desc f = O.desc (exprToColumn @b @O.PGInt8 . f)

orderNulls
  :: DBOrd b
  => ((a -> Expr b) -> O.Order a)
  -> OrderNulls
  -> (a -> Expr (Maybe b))
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
