{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Query.Evaluate
  ( Evaluate
  , eval
  , evaluate
  )
where

-- base
import Data.Kind ( Type )
import Data.Monoid ( Endo ( Endo ), appEndo )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query( Query ) )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( unpackspec )

-- semigroupoids
import Data.Functor.Apply ( Apply )
import Data.Functor.Bind ( Bind, (>>-) )

-- transformers
import Control.Monad.Trans.State.Strict ( State, get, put, runState )


type Evaluations :: Type
data Evaluations = Evaluations
  { tag :: !Opaleye.Tag
  , bindings :: !(Endo (Opaleye.Bindings Opaleye.PrimExpr))
  }


-- | Some PostgreSQL functions, such as 'Rel8.nextval', have side effects,
-- breaking the referential transparency we would otherwise enjoy.
--
-- To try to recover our ability to reason about such expressions, 'Rel8'
-- provides the 'Evaluate' monad, which allows us to control the evaluation
-- order of side-effects by sequencing them monadically.
type Evaluate :: Type -> Type
newtype Evaluate a = Evaluate (State Evaluations a)
  deriving newtype (Functor, Apply, Applicative, Monad)


instance Bind Evaluate where
  (>>-) = (>>=)


-- | 'eval' takes expressions that could potentially have side effects and
-- lifts them into the 'Evaluate' monad. The resulting expressions has no side
-- effetcs and can safely be reused.
eval :: Table Expr a => a -> Evaluate a
eval a = Evaluate $ do
  Evaluations {tag, bindings} <- get
  let
    tag' = Opaleye.next tag
    (a', bindings') = Opaleye.run $
      Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "eval" tag') a
  put Evaluations {tag = tag', bindings = bindings <> Endo (bindings' ++)}
  pure a'


-- | 'evaluate' runs an 'Evaluate' inside the 'Query' monad.
evaluate :: Evaluate a -> Query a
evaluate (Evaluate m) = Query $ Opaleye.QueryArr $ \(_, query, tag) ->
  case runState m (Evaluations tag mempty) of
    (a, Evaluations {tag = tag', bindings}) ->
      (a, Opaleye.Rebind True (appEndo bindings mempty) query, tag')
