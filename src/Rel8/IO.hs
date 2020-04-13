{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}

module Rel8.IO where

import Control.Applicative ( Const(..) )
import Data.Indexed.Functor.Constrained ( HConstrained(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Monoid ( Any(..) )
import Database.PostgreSQL.Simple ( Connection )
import Database.PostgreSQL.Simple.FromField ( FromField )
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import Rel8.Expr
import Rel8.Query
import Rel8.Table


select :: (Table a, All (Pattern a) FromField) => Connection -> ( forall x. Query x (Expr a) ) -> IO [a]
select _c _q = undefined


queryRunner :: Table a  => Opaleye.QueryRunner (Expr a) a
queryRunner = Opaleye.QueryRunner unpackspec (const rowParser) hasColumns


unpackspec :: Opaleye.Unpackspec (Expr a) ()
unpackspec = undefined


hasColumns :: HTraversable (Pattern a) => Expr a -> Bool
hasColumns (Expr f) = getAny $ getConst $ htraverse (\_ -> Const $ Any True) f
