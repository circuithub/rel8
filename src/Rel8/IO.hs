{-# language FlexibleContexts #-}
{-# language TypeApplications #-}

module Rel8.IO where

import Control.Monad.Trans.State.Strict ( runState )
import Data.Coerce ( coerce )
import Data.Functor.Identity
import Database.PostgreSQL.Simple ( Connection, queryWith_ )
import Database.PostgreSQL.Simple.FromField ( FromField, fromField )
import Database.PostgreSQL.Simple.FromRow ( RowParser, field )
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import Rel8.Expr
import Rel8.Primitive
import Rel8.Query
import Rel8.Table


select :: Connection -> Query () (Expr a) -> IO [a]
select c q = undefined


queryRunner :: (TraversePrimitives (Pattern a), Table a, All FromField (Pattern a)) => Opaleye.QueryRunner (Expr a) a
queryRunner = Opaleye.QueryRunner unpackspec rowParser hasColumns


unpackspec :: Opaleye.Unpackspec (Expr a) ()
unpackspec = undefined


rowParser :: (TraversePrimitives (Pattern a), Table a, All FromField (Pattern a)) => Expr a -> RowParser a
rowParser (Expr f) =
  fmap to $ traversePrimitives @_ @FromField (\_ -> PrimIdentity <$> field) f


hasColumns :: Expr a -> Bool
hasColumns = undefined
