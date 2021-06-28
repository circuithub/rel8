{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Statement.Manipulation.DSL
  ( module Rel8.Statement.Manipulation.DSL
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Foldable ( toList )
import Data.Function ( on )
import Data.Functor.Compose ( Compose( Compose ) )
import Data.Int ( Int64 )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Data.Monoid ( Any( Any ) )
import Prelude

-- free
import Control.Applicative.Free

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), equals, parens, text )

-- rel8
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.Manipulation ( Set, Where, ppWhere )
import Rel8.Table.Opaleye ( castTable, exprs, namedExprs, view )
import Rel8.Table.Serialize ( Serializable )


type ReturningF :: Type -> Type -> Type
data ReturningF exprs a where
  NumberOfRowsAffected :: ReturningF exprs Int64
  Returning :: Serializable returning a
    => (exprs -> returning)
    -> ReturningF exprs [a]


type Returning :: (Type -> Type -> Type) -> Constraint
class Returning m where
  numberOfRowsAffected :: m exprs Int64
  returning :: Serializable returning a => (exprs -> returning) -> m exprs [a]


type Restrict :: (Type -> Type -> Type) -> Constraint
class Restrict m where
  restrict :: Where exprs -> m exprs ()


type UpdateF :: Type -> Type
data UpdateF exprs = UpdateF
  { set_ :: Set exprs
  , where_ :: Where exprs
  }


instance Semigroup (UpdateF exprs) where
  a <> b = UpdateF
    { set_ = ((.) `on` set_) b a
    , where_ = (liftA2 (*>) `on` where_) a b
    }


instance Monoid (UpdateF exprs) where
  mempty = UpdateF
    { set_ = id
    , where_ = \_ ->  pure ()
    }


newtype Update exprs a = Update (UpdateF exprs, Ap (ReturningF exprs) a)
  deriving (Functor, Applicative) via
    Compose ((,) (UpdateF exprs)) (Ap (ReturningF exprs))


instance Returning Update where
  numberOfRowsAffected = Update (pure (liftAp NumberOfRowsAffected))
  returning projection = Update (pure (liftAp (Returning projection)))


instance Restrict Update where
  restrict condition = Update (UpdateF id condition, pure ())


set :: Set exprs -> Update exprs ()
set f = Update (UpdateF f (const (pure ())), pure ())


ppUpdate :: Selects names exprs
  => TableSchema names -> Update exprs () -> Maybe Doc
ppUpdate target (Update (UpdateF {..}, returning)) = do
  condition <- ppWhere target where_
  pure $
    text "UPDATE" <+>
    ppTable target $$
    ppSet target set_ $$
    condition $$ ppReturning target returning


ppSet :: Selects names exprs => TableSchema names -> Set exprs -> Doc
ppSet TableSchema {columns} f =
  text "SET" <+> Opaleye.commaV ppAssign (toList assigns)
  where
    assigns =
      fmap Opaleye.sqlExpr <$> namedExprs columns (f (view columns))
    ppAssign (column, expr) =
      ppColumn column <+> equals <+> Opaleye.ppSqlExpr expr


ppReturning :: forall names exprs a. Selects names exprs => TableSchema names -> Ap (ReturningF exprs) a -> Doc
ppReturning TableSchema {columns} returnings =
  case runAp_ projections returnings of
    Nothing -> mempty
    Just columns ->
      text "RETURNING" <+> Opaleye.commaV Opaleye.ppSqlExpr (toList sqlExprs)
      where
        sqlExprs = Opaleye.sqlExpr <$> columns
  where
    projections :: ReturningF exprs x -> Maybe (NonEmpty Opaleye.PrimExpr)
    projections = \case
      NumberOfRowsAffected -> Nothing
      Returning projection -> Just (exprs (castTable (projection (view columns))))
