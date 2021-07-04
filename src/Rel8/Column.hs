{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Column
  ( Column, AColumn(..)
  , TColumn
  )
where

-- base
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Col( A ) )
import Rel8.Expr ( Expr, Col( E ) )
import Rel8.FCF ( Eval, Exp )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.HTable.Identity ( HIdentity(..), HType )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name(..), Col( N ) )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( Reify, Col(..) )
import Rel8.Schema.Result ( Col( R ), Result, absurd )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify, coherence, congruence
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


-- | This type family is used to specify columns in 'Rel8able's. In @Column f
-- a@, @f@ is the context of the column (which should be left polymorphic in
-- 'Rel8able' definitions), and @a@ is the type of the column.
type Column :: K.Context -> Type -> Type
type family Column context a where
  Column (Reify context) a = AColumn context a
  Column Aggregate       a = Aggregate a
  Column Expr            a = Expr a
  Column Name            a = Name a
  Column Result          a = a


type AColumn :: K.Context -> Type -> Type
newtype AColumn context a = AColumn (Column context a)


instance (Reifiable context, Sql DBType a) =>
  Table (Reify context) (AColumn context a)
 where
  type Context (AColumn context a) = Reify context
  type Columns (AColumn context a) = HType a
  type Unreify (AColumn context a) = Column context a

  fromColumns (HType (Reify a)) = sfromColumn contextSing a
  toColumns = HType . Reify . stoColumn contextSing
  reify _ = AColumn
  unreify _ (AColumn a) = a

  coherence Refl = case contextSing @context of
    SAggregate -> const Refl
    SExpr -> const Refl
    SName -> const Refl
    SResult -> absurd
    SReify _ -> const Refl

  congruence Refl = case contextSing @context of
    SAggregate -> const Refl
    SExpr -> const Refl
    SName -> const Refl
    SResult -> absurd
    SReify _ -> const Refl


instance (Reifiable context, Reifiable context',  Sql DBType a) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AColumn context a)
    (AColumn context' a)


sfromColumn :: ()
  => SContext context
  -> Col context ('Spec a)
  -> AColumn context a
sfromColumn = \case
  SAggregate -> \(A a) -> AColumn a
  SExpr -> \(E a) -> AColumn a
  SName -> \(N a) -> AColumn a
  SResult -> \(R a) -> AColumn a
  SReify context -> \(Reify a) -> AColumn (sfromColumn context a)


stoColumn :: ()
  => SContext context
  -> AColumn context a
  -> Col context ('Spec a)
stoColumn = \case
  SAggregate -> \(AColumn a) -> A a
  SExpr -> \(AColumn a) -> E a
  SName -> \(AColumn a) -> N a
  SResult -> \(AColumn a) -> R a
  SReify context -> \(AColumn a) -> Reify (stoColumn context a)


data TColumn :: K.Context -> Type -> Exp Type
type instance Eval (TColumn f a) = Column f a
