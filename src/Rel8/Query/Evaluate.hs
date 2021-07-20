{-# language FlexibleContexts #-}
{-# language TupleSections #-}

module Rel8.Query.Evaluate
  ( evaluate
  )
where

-- base
import Control.Monad ( (>=>) )
import Data.Foldable ( foldl' )
import Data.List.NonEmpty ( NonEmpty( (:|) ), nonEmpty )
import Data.Monoid ( Any( Any ) )
import Prelude hiding ( undefined )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (&&.) )
import Rel8.Expr.Opaleye ( fromPrimExpr )
import Rel8.Query ( Query( Query ) )
import Rel8.Query.Rebind ( rebind )
import Rel8.Table ( Table )
import Rel8.Table.Bool ( case_ )
import Rel8.Table.Undefined ( undefined )


-- | 'evaluate' takes expressions that could potentially have side effects and
-- \"runs\" them in the 'Query' monad. The returned expressions have no side
-- effects and can safely be reused.
evaluate :: Table Expr a => a -> Query a
evaluate = laterally >=> rebind "eval"


laterally :: Table Expr a => a -> Query a
laterally a = Query $ \bindings -> pure $ (Any True,) $
  case nonEmpty bindings of
    Nothing -> a
    Just bindings' -> case_ [(condition, a)] undefined
      where
        condition = foldl1' (&&.) (fmap go bindings')
          where
            go = fromPrimExpr . Opaleye.UnExpr Opaleye.OpIsNotNull


foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (a :| as) = foldl' f a as
