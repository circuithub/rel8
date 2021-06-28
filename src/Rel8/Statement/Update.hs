{-# language DerivingVia #-}
{-# language MonoLocalBinds #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Statement.Update
  ( Update(..)
  , update
  , Set
  , set
  , UpdateF(..)
  , ppUpdate
  , ppSet
  )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Exception ( throwIO )
import Data.Foldable ( toList )
import Data.Function ( on )
import Data.Functor.Compose ( Compose( Compose ) )
import Data.Kind ( Type )
import Prelude

-- free
import Control.Applicative.Free ( Ap, liftAp )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), equals, text )

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import Rel8.Schema.Name ( Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.Returning
  ( Returning
  , ReturningF(..), decodeReturning, emptyReturning, ppReturning
  )
import qualified Rel8.Statement.Returning
import Rel8.Statement.Where ( Restrict, restrict, Where, ppWhere )
import Rel8.Table.Opaleye ( exprsWithNames, view )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | The @SET@ part of an @UPDATE@ (or @ON CONFLICT DO UPDATE@) statement.
type Set :: Type -> Type
type Set expr = expr -> expr


ppSet :: Selects names exprs => TableSchema names -> Set exprs -> Doc
ppSet TableSchema {columns} f =
  text "SET" <+> Opaleye.commaV ppAssign (toList assigns)
  where
    assigns =
      fmap Opaleye.sqlExpr <$> exprsWithNames columns (f (view columns))
    ppAssign (column, expr) =
      ppColumn column <+> equals <+> Opaleye.ppSqlExpr expr


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


-- | An 'Applicative' that builds an @UPDATE@ statement.
type Update :: Type -> Type -> Type
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
  => TableSchema names -> Update exprs a -> Maybe Doc
ppUpdate target (Update (UpdateF {..}, returning)) = do
  condition <- ppWhere target where_
  pure $
    text "UPDATE" <+>
    ppTable target $$
    ppSet target set_ $$
    condition $$ ppReturning target returning


-- | Run an 'Update' statement.
update :: Selects names exprs
  => Connection -> TableSchema names -> Update exprs a -> IO a
update connection target u@(Update (_, returning)) =
  case show <$> ppUpdate target u of
    Nothing -> pure (emptyReturning returning)
    Just sql ->
      Hasql.run session connection >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = decodeReturning returning
        prepare = False
