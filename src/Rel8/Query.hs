{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Query where

import Control.Monad
import Data.Proxy
import Database.PostgreSQL.Simple ( Connection )
import qualified Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import Rel8.Column
import Rel8.Expr
import Rel8.MonadQuery
import Rel8.Top
import Rel8.ZipLeaves
import {-# source #-} Rel8.FromRow


newtype Query a = Query ( Opaleye.Query a )
  deriving ( Functor, Applicative )


instance Monad Query where
  return = pure
  Query ( Opaleye.QueryArr f ) >>= g = Query $ Opaleye.QueryArr \input ->
    case ( f input ) of
      ( a, primQuery, tag ) ->
        case g a of
          Query ( Opaleye.QueryArr h ) ->
            h ( (), primQuery, tag )


instance MonadQuery Query where
  liftOpaleye =
    Query

  toOpaleye ( Query q ) =
    q


-- | Run a @SELECT@ query, returning all rows.
select
  :: forall row haskell
   . ( CanZipLeaves row row Top
     , ZipLeaves row row ( Expr Query ) ( Expr Query )
     , FromRow row haskell
     )
  => Connection -> Query row -> IO [ haskell ]
select c ( Query query ) =
  Opaleye.runSelectExplicit fromFields c query

  where

    fromFields :: Opaleye.FromFields row haskell
    fromFields =
      Opaleye.QueryRunner ( void unpackspec ) rowParser ( const True )


    unpackspec :: Opaleye.Unpackspec row row
    unpackspec =
      Opaleye.Unpackspec $ Opaleye.PackMap \f row ->
        zipLeaves
          ( Proxy @Top )
          ( \( C x ) _ -> C . Expr <$> f ( toPrimExpr x ) )
          row
          row


showSQL
  :: forall a
   . ( CanZipLeaves a a Top, ZipLeaves a a ( Expr Query ) ( Expr Query ) )
  => Query a -> Maybe String
showSQL ( Query opaleye ) =
  Opaleye.showSqlExplicit unpackspec opaleye

  where

    unpackspec :: Opaleye.Unpackspec a a
    unpackspec =
      Opaleye.Unpackspec $ Opaleye.PackMap \f row ->
        zipLeaves
          ( Proxy @Top )
          ( \( C expr ) _ -> C . Expr <$> f ( toPrimExpr expr ) )
          row
          row
