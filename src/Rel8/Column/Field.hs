{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Column.Field
  ( Field, AField(..)
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Col( A ) )
import Rel8.Expr ( Expr, Col( E ) )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Kind.Defaulting ( Defaulting, KnownDefaulting )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.Insert ( Col( I ), Create(..), Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name(..), Col( N ) )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( Reify, Col(..) )
import Rel8.Schema.Result ( Col( R ), Result )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


type Field :: K.Context -> Defaulting -> Type -> Type
type family Field context defaulting a where
  Field (Reify context) defaulting  a = AField context defaulting a
  Field Aggregate       _defaulting a = Aggregate a
  Field Expr            _defaulting a = Expr a
  Field Insert          defaulting  a = Create defaulting a
  Field Name            _defaulting a = Name a
  Field Result          _defaulting a = a


type AField :: K.Context -> Defaulting -> Type -> Type
newtype AField context defaulting a = AField (Field context defaulting a)


instance (Reifiable context, KnownDefaulting defaulting, Sql DBType a) =>
  Table (Reify context) (AField context defaulting a)
 where
  type Context (AField context defaulting a) = Reify context
  type Columns (AField context defaulting a) = HIdentity ('Spec '[] defaulting a)
  type Unreify (AField context defaulting a) = Field context defaulting a

  fromColumns (HIdentity (Reify a)) = sfromColumn contextSing a
  toColumns = HIdentity . Reify . stoColumn contextSing
  reify _ = AField
  unreify _ (AField a) = a


instance
  ( Reifiable context, Reifiable context'
  , KnownDefaulting defaulting, Sql DBType a
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AField context defaulting a)
    (AField context' defaulting a)


sfromColumn :: ()
  => SContext context
  -> Col context ('Spec labels defaulting a)
  -> AField context defaulting a
sfromColumn = \case
  SAggregate -> \(A a) -> AField a
  SExpr -> \(E a) -> AField a
  SInsert -> \(I a) -> AField a
  SName -> \(N a) -> AField a
  SResult -> \(R a) -> AField a
  SReify context -> \(Reify a) -> AField (sfromColumn context a)


stoColumn :: ()
  => SContext context
  -> AField context defaulting a
  -> Col context ('Spec labels defaulting a)
stoColumn = \case
  SAggregate -> \(AField a) -> A a
  SExpr -> \(AField a) -> E a
  SInsert -> \(AField a) -> I a
  SName -> \(AField a) -> N a
  SResult -> \(AField a) -> R a
  SReify context -> \(AField a) -> Reify (stoColumn context a)
