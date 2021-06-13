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
import Rel8.Kind.Necessity
  ( Necessity( Required, Optional )
  , SNecessity( SRequired, SOptional )
  , KnownNecessity, necessitySing
  )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.Insert ( Insert, Insertion(..), Col( I ) )
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


type Field :: K.Context -> Necessity -> Type -> Type
type family Field context necessity a where
  Field (Reify context) necessity  a = AField context necessity a
  Field Aggregate       _necessity a = Aggregate (Expr a)
  Field Expr            _necessity a = Expr a
  Field Insert          'Required  a = Expr a
  Field Insert          'Optional  a = Maybe (Expr a)
  Field Name            _necessity a = Name a
  Field Result          _necessity a = a


type AField :: K.Context -> Necessity -> Type -> Type
newtype AField context necessity a = AField (Field context necessity a)


instance (Reifiable context, KnownNecessity necessity, Sql DBType a) =>
  Table (Reify context) (AField context necessity a)
 where
  type Context (AField context necessity a) = Reify context
  type Columns (AField context necessity a) = HIdentity ('Spec '[] necessity a)
  type Unreify (AField context necessity a) = Field context necessity a

  fromColumns (HIdentity (Reify a)) = sfromColumn contextSing necessitySing a
  toColumns = HIdentity . Reify . stoColumn contextSing necessitySing
  reify _ = AField
  unreify _ (AField a) = a


instance
  ( Reifiable context, Reifiable context'
  , KnownNecessity necessity, Sql DBType a
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AField context necessity a)
    (AField context' necessity a)


sfromColumn :: ()
  => SContext context
  -> SNecessity necessity
  -> Col context ('Spec labels necessity a)
  -> AField context necessity a
sfromColumn = \case
  SAggregate -> \_ (A a) -> AField a
  SExpr -> \_ (E a) -> AField a
  SInsert -> \case
    SRequired -> \case
      I (Value a) -> AField a
    SOptional -> \case
      I Default -> AField Nothing
      I (Value a) -> AField (Just a)
  SName -> \_ (N a) -> AField a
  SResult -> \_ (R a) -> AField a
  SReify context ->
    \necessity (Reify a) -> AField (sfromColumn context necessity a)


stoColumn :: ()
  => SContext context
  -> SNecessity necessity
  -> AField context necessity a
  -> Col context ('Spec labels necessity a)
stoColumn = \case
  SAggregate -> \_ (AField a) -> A a
  SExpr -> \_ (AField a) -> E a
  SInsert -> \case
    SRequired -> \(AField a) -> I (Value a)
    SOptional -> \(AField ma) -> I $ maybe Default Value ma
  SName -> \_ (AField a) -> N a
  SResult -> \_ (AField a) -> R a
  SReify context ->
    \necessity (AField a) -> Reify (stoColumn context necessity a)
