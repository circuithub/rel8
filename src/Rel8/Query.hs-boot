{-# language ExistentialQuantification #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Query
  ( Query( Query )
  )
where

-- base
import Data.Kind ( Type )
import Data.Monoid ( Endo )
import Prelude ()

-- opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- transformers
import Control.Monad.Trans.State.Strict ( State )


type Query :: Type -> Type
data Query a = forall x. Query (State Opaleye.Tag (Endo Opaleye.PrimQuery, x), x -> Opaleye.Select a)
