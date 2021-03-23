module Rel8.Query.Filter
  ( filter
  , where_
  )
where

-- base
import Prelude hiding ( filter )

-- opaleye
import qualified Opaleye.Operators as Opaleye

-- profunctors
import Data.Profunctor ( lmap )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( toColumn, toPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye )


filter :: (a -> Expr Bool) -> a -> Query a
filter f a = a <$ where_ (f a)


where_ :: Expr Bool -> Query ()
where_ condition =
  fromOpaleye $ lmap (\_ -> toColumn $ toPrimExpr condition) Opaleye.restrict
