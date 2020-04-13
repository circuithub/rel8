{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}

module Rel8.IO where

import Control.Applicative ( Const(..) )
import Control.Monad ( void )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Monoid ( Any(..) )
import Database.PostgreSQL.Simple ( Connection )
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.RunQuery as Opaleye
import Rel8.Expr
import Rel8.Query
import Rel8.Table


select :: Table a => Connection -> ( forall x. Query x (Expr a) ) -> IO [a]
select c = Opaleye.runQueryExplicit queryRunner c . toOpaleye


queryRunner :: Table a  => Opaleye.QueryRunner (Expr a) a
queryRunner = Opaleye.QueryRunner (void unpackspec) (const rowParser) hasColumns


hasColumns :: HTraversable (Pattern a) => Expr a -> Bool
hasColumns (Expr f) = getAny $ getConst $ htraverse (\_ -> Const $ Any True) f
