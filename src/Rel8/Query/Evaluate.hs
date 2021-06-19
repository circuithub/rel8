{-# language FlexibleContexts #-}
{-# language TupleSections #-}

module Rel8.Query.Evaluate
  ( evaluate
  , rebind
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
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (&&.) )
import Rel8.Expr.Opaleye ( fromPrimExpr )
import Rel8.Query ( Query( Query ) )
import Rel8.Table ( Table )
import Rel8.Table.Bool ( case_ )
import Rel8.Table.Opaleye ( unpackspec )
import Rel8.Table.Undefined


-- | 'evaluate' takes expressions that could potentially have side effects and
-- \"runs\" them in the 'Query' monad. The returned expressions have no side
-- effects and can safely be reused.
evaluate :: Table Expr a => a -> Query a
evaluate = laterally >=> rebind


laterally :: Table Expr a => a -> Query a
laterally a = Query $ \bindings -> pure $ (Any True,) $
  case nonEmpty bindings of
    Nothing -> a
    Just bindings' -> case_ [(condition, a)] undefined
      where
        condition = foldl1' (&&.) (fmap go bindings')
          where
            go = fromPrimExpr . Opaleye.UnExpr Opaleye.OpIsNotNull


-- | 'rebind' takes some expressions, and binds each of them to a new
-- variable in the SQL. The @a@ returned consists only of these
-- variables. It's essentially a @let@ binding for Postgres expressions.
rebind :: Table Expr a => a -> Query a
rebind a = Query $ \_ -> Opaleye.QueryArr $ \(_, query, tag) ->
  let
    tag' = Opaleye.next tag
    (a', bindings) = Opaleye.run $
      Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "eval" tag') a
  in
    ((mempty, a'), Opaleye.Rebind True bindings query, tag')


foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (a :| as) = foldl' f a as
