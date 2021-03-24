{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Insert
  ( Insert(..)
  , OnConflict(..)
  , Col( RequiredInsert, OptionalInsert )
  , Insertion(..)
  , Inserts
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Necessity ( Necessity(Optional, Required) )
import Rel8.Opaque ( Opaque, Opaque1 )
import Rel8.Schema.Context ( Interpretation(..) )
import Rel8.Schema.Context.Label ( Labelable(..) )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, encodeTag, decodeTag, nullifier, unnullifier
  , runTag, unnull
  )
import Rel8.Schema.HTable.Type ( HType(HType) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name, Selects )
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( SSpec(SSpec, nullability), Spec(Spec) )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Returning ( Returning )
import Rel8.Table ( Table(..) )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


-- | @OnConflict@ allows you to add an @ON CONFLICT@ clause to an @INSERT@
-- statement.
data OnConflict
  = Abort     -- ^ @ON CONFLICT ABORT@
  | DoNothing -- ^ @ON CONFLICT DO NOTHING@


-- | The constituent parts of a SQL @INSERT@ statement.
type Insert :: K.Context
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
    RequiredInsert :: Expr a -> Col Insert ('Spec labels 'Required db a)
    OptionalInsert :: Maybe (Expr a) -> Col Insert ('Spec labels 'Optional db a)


type Insertion :: K.Context
newtype Insertion a = Insertion (Expr a)


instance Sql DBType a => Table Insert (Insertion a) where
  type Columns (Insertion a) = HType a
  type Context (Insertion a) = Insert

  toColumns (Insertion a) = HType (RequiredInsert a)
  fromColumns (HType (RequiredInsert a)) = Insertion a


instance Sql DBType a =>
  Recontextualize Aggregate Insert (Aggregate (Expr a)) (Insertion a)


instance Sql DBType a => Recontextualize Expr Insert (Expr a) (Insertion a)


instance Sql DBType a =>
  Recontextualize Identity Insert (Identity a) (Insertion a)


instance Sql DBType a =>
  Recontextualize Insert Aggregate (Insertion a) (Aggregate (Expr a))


instance Sql DBType a => Recontextualize Insert Expr (Insertion a) (Expr a)


instance Sql DBType a =>
  Recontextualize Insert Identity (Insertion a) (Identity a)


instance Sql DBType a => Recontextualize Insert Insert (Insertion a) (Insertion a)


instance Sql DBType a => Recontextualize Insert Name (Insertion a) (Name a)


instance Sql DBType a => Recontextualize Name Insert (Name a) (Insertion a)


instance Labelable Insert where
  labeler = \case
    RequiredInsert a -> RequiredInsert a
    OptionalInsert ma -> OptionalInsert ma

  unlabeler = \case
    RequiredInsert a -> RequiredInsert a
    OptionalInsert ma -> OptionalInsert ma


instance Nullifiable Insert where
  encodeTag = RequiredInsert
  decodeTag (RequiredInsert a) = a

  nullifier tag SSpec {nullability} = \case
    RequiredInsert a -> RequiredInsert $ runTag nullability tag a
    OptionalInsert ma -> OptionalInsert $ runTag nullability tag <$> ma

  unnullifier _ SSpec {nullability} = \case
    RequiredInsert a -> RequiredInsert $ unnull nullability a
    OptionalInsert ma -> OptionalInsert $ unnull nullability <$> ma

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}



-- | @Inserts a b@ means that the columns in @a@ are compatible for inserting
-- with the table @b@.
type Inserts :: Type -> Type -> Constraint
class Recontextualize Expr Insert exprs inserts => Inserts exprs inserts
instance Recontextualize Expr Insert exprs inserts => Inserts exprs inserts
instance {-# OVERLAPPING #-} Inserts (Opaque1 Expr Opaque) (Opaque1 Insert Opaque)
