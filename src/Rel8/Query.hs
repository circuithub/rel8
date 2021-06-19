{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language ExistentialQuantification #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Query
  ( Query( Query )
  )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Monad ( liftM2 )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Monoid ( Endo( Endo ) )
import Prelude

-- opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- rel8
import Rel8.Query.Set ( unionAll )
import Rel8.Query.Values ( values )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )

-- transformers
import Control.Monad.Trans.State.Strict ( State, runState )


-- | The @Query@ monad allows you to compose a @SELECT@ query. This monad has
-- semantics similar to the list (@[]@) monad.
type Query :: Type -> Type
data Query a = Query (State Opaleye.Tag (Endo Opaleye.PrimQuery, Opaleye.Select a))


instance Functor Query where
  fmap f (Query s) = Query ((fmap . fmap . fmap) f s)


instance Apply Query where
  (<.>) = (<*>)


instance Applicative Query where
  pure a = Query (pure (mempty, pure a))
  liftA2 = liftM2


instance Bind Query where
  (>>-) = (>>=)


instance Monad Query where
  Query s >>= f = Query
    ( flip (fmap . fmap) s $
     \qff -> Opaleye.QueryArr $ \i ->
        let
          Opaleye.QueryArr qf = qff
          (a, query, tag) = qf i
        in
          case f a of
            Query s' ->
              let
                ((Endo modify, qff'), tag') = runState s' tag
                Opaleye.QueryArr qf' = qff'
                lquery = modify query
                (b, rquery, tag'') = qf' ((), Opaleye.Unit, tag')
                query'' =
                  Opaleye.Product
                    ((Opaleye.NonLateral, lquery) :| [(Opaleye.Lateral, rquery)])
                    []
              in
                (b, query'', tag'')
    )


-- | '<|>:' = 'unionAll'.
instance AltTable Query where
  (<|>:) = unionAll


-- | 'emptyTable' = 'values' @[]@.
instance AlternativeTable Query where
  emptyTable = values []
