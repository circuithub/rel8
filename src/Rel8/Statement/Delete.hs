{-# language DerivingVia #-}
{-# language MonoLocalBinds #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Statement.Delete
  ( Delete
  , delete
  , ppDelete
  )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Exception ( throwIO )
import Data.Functor.Compose ( Compose( Compose ) )
import Data.Function ( on )
import Data.Kind ( Type )
import Prelude

-- free
import Control.Applicative.Free ( Ap, liftAp )

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), text )

-- rel8
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema, ppTable )
import Rel8.Statement.Returning
  ( Returning
  , ReturningF(..), decodeReturning, emptyReturning, ppReturning
  )
import qualified Rel8.Statement.Returning
import Rel8.Statement.Where ( Restrict, restrict, Where, ppWhere )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


type DeleteF :: Type -> Type
data DeleteF exprs = DeleteF
  { where_ :: Where exprs
  }


instance Semigroup (DeleteF exprs) where
  a <> b = DeleteF
    { where_ = (liftA2 (*>) `on` where_) a b
    }


instance Monoid (DeleteF exprs) where
  mempty = DeleteF
    { where_ = \_ -> pure ()
    }


-- | An 'Applicative' that builds a @DELETE@ statement.
type Delete :: Type -> Type -> Type
newtype Delete exprs a = Delete (DeleteF exprs, Ap (ReturningF exprs) a)
  deriving (Functor, Applicative) via
    Compose ((,) (DeleteF exprs)) (Ap (ReturningF exprs))


instance Returning Delete where
  numberOfRowsAffected = Delete (pure (liftAp NumberOfRowsAffected))
  returning projection = Delete (pure (liftAp (Returning projection)))


instance Restrict Delete where
  restrict condition = Delete (DeleteF condition, pure ())


ppDelete :: Selects names exprs
  => TableSchema names -> Delete exprs a -> Maybe Doc
ppDelete from (Delete (DeleteF {..}, returning)) = do
  condition <- ppWhere from where_
  pure $ text "DELETE FROM" <+> ppTable from
    $$ condition
    $$ ppReturning from returning


-- | Run a 'Delete' statement.
delete :: Selects names exprs
  => Connection -> TableSchema names -> Delete exprs a -> IO a
delete connection from d@(Delete (_, returning)) =
  case show <$> ppDelete from d of
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
