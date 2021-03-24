{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Query
  ( Query( Query )
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Select as Opaleye

-- rel8
import Rel8.Query.Set ( unionAll )
import Rel8.Query.Values ( values )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )

-- semigroupoids
import Data.Functor.Apply ( Apply, WrappedApplicative(..) )
import Data.Functor.Bind ( Bind, (>>-) )


-- | The @Query@ monad allows you to compose a @SELECT@ query. This monad has
-- semantics similar to the list (@[]@) monad.
type Query :: Type -> Type
newtype Query a = Query (Opaleye.Select a)
  deriving newtype (Functor, Applicative, Monad)
  deriving Apply via (WrappedApplicative Opaleye.Select)


instance Bind Query where
  (>>-) = (>>=)


-- | '<|>:' = 'unionAll'.
instance AltTable Query where
  (<|>:) = unionAll


-- | 'emptyTable' = 'values' @[]@.
instance AlternativeTable Query where
  emptyTable = values []
