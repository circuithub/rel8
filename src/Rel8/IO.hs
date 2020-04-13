{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}

module Rel8.IO where

import Data.Dict ( Dict(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Indexed.Functor.Constrained ( HConstrained(..) )
import Data.Indexed.Functor.Representable ( hzipWith )
import Data.Indexed.Functor.Traversable ( hsequence )
import Data.Proxy ( Proxy(..) )
import Database.PostgreSQL.Simple ( Connection )
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.FromRow ( RowParser, field )
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import Rel8.Expr
import Rel8.Query
import Rel8.Table


select :: (Table a, All (Pattern a) FromField) => Connection -> ( forall x. Query x (Expr a) ) -> IO [a]
select _c _q = undefined


queryRunner :: (Table a, All (Pattern a) FromField)  => Opaleye.QueryRunner (Expr a) a
queryRunner = Opaleye.QueryRunner unpackspec rowParser hasColumns


unpackspec :: Opaleye.Unpackspec (Expr a) ()
unpackspec = undefined


rowParser :: (Table a, All (Pattern a) FromField) => Expr a -> RowParser a
rowParser (Expr f) =
  fmap to $ hsequence $
  hzipWith (\(Compose Dict) _ -> Compose $ Identity <$> field) fromFieldDicts f
  where fromFieldDicts = hconstrained (Proxy @FromField)


hasColumns :: Expr a -> Bool
hasColumns = undefined
