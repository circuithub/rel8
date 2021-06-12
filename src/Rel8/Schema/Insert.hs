{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Insert
  ( Insert(..)
  , OnConflict(..)
  , Col( I, unI )
  , Inserts
  , Create(..)
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Necessity ( Necessity(Optional, Required), KnownNecessity )
import Rel8.Schema.Context ( Interpretation(..) )
import Rel8.Schema.Context.Label ( Labelable(..) )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, encodeTag, decodeTag, nullifier, unnullifier
  , runTag, unnull
  )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.Name ( Name, Selects )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( notReify )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( SSpec(SSpec, nullity), Spec(Spec) )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Returning ( Returning )
import Rel8.Table
  ( Table, Context, Columns, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Tag ( Tag(..), fromExpr )
import Rel8.Type ( DBType )


-- | @OnConflict@ allows you to add an @ON CONFLICT@ clause to an @INSERT@
-- statement.
data OnConflict
  = Abort     -- ^ @ON CONFLICT ABORT@
  | DoNothing -- ^ @ON CONFLICT DO NOTHING@


-- | The constituent parts of a SQL @INSERT@ statement.
type Insert :: k -> Type
data Insert a where
  Insert :: (Selects names exprs, Inserts exprs inserts) =>
    { into :: TableSchema names
      -- ^ Which table to insert into.
    , rows :: [inserts]
      -- ^ The rows to insert.
    , onConflict :: OnConflict
      -- ^ What to do if the inserted rows conflict with data already in the
      -- table.
    , returning :: Returning names a
      -- ^ What information to return on completion.
    }
    -> Insert a


instance Interpretation Insert where
  data Col Insert _spec where
    I :: {unI :: !(Create necessity a)} -> Col Insert ('Spec labels necessity a)


type Create :: Necessity -> Type -> Type
data Create necessity a where
  Default :: Create 'Optional a
  Value :: Expr a -> Create necessity a


instance (KnownNecessity necessity, Sql DBType a) =>
  Table Insert (Create necessity a)
 where
  type Columns (Create necessity a) = HIdentity ('Spec '[] necessity a)
  type Context (Create necessity a) = Insert

  toColumns = HIdentity . I
  fromColumns (HIdentity (I a)) = a
  reify = notReify
  unreify = notReify


instance Sql DBType a =>
  Recontextualize Aggregate Insert (Aggregate a) (Create 'Required a)


instance Sql DBType a => Recontextualize Expr Insert (Expr a) (Create 'Required a)


instance Sql DBType a =>
  Recontextualize Result Insert (Identity a) (Create 'Required a)


instance Sql DBType a =>
  Recontextualize Insert Aggregate (Create 'Required a) (Aggregate a)


instance Sql DBType a => Recontextualize Insert Expr (Create 'Required a) (Expr a)


instance Sql DBType a =>
  Recontextualize Insert Result (Create 'Required a) (Identity a)


instance Sql DBType a => Recontextualize Insert Insert (Create 'Required a) (Create 'Required a)


instance Sql DBType a => Recontextualize Insert Name (Create 'Required a) (Name a)


instance Sql DBType a => Recontextualize Name Insert (Name a) (Create 'Required a)


instance Labelable Insert where
  labeler (I a) = I a
  unlabeler (I a) = I a


instance Nullifiable Insert where
  encodeTag = I . Value . expr

  decodeTag (I (Value a)) = fromExpr a

  nullifier Tag {expr} test SSpec {nullity} = \case
    I Default -> I Default
    I (Value a) -> I $ Value $ runTag nullity condition a
    where
      condition = test expr

  unnullifier SSpec {nullity} = \case
    I Default -> I Default
    I (Value a) -> I $ Value $ unnull nullity a

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


-- | @Inserts a b@ means that the columns in @a@ are compatible for inserting
-- with the table @b@.
type Inserts :: Type -> Type -> Constraint
class Recontextualize Expr Insert exprs inserts => Inserts exprs inserts
instance Recontextualize Expr Insert exprs inserts => Inserts exprs inserts
