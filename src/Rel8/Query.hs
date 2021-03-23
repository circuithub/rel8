{-# language DerivingStrategies #-}
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


type Query :: Type -> Type
newtype Query a = Query (Opaleye.Select a)
  deriving newtype (Functor, Applicative, Monad)


instance AltTable Query where
  (<|>:) = unionAll


instance AlternativeTable Query where
  emptyTable = values []
