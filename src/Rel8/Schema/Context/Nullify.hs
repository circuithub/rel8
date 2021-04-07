{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context.Nullify
  ( Nullifiable( ConstrainTag, encodeTag, decodeTag, nullifier, unnullifier )
  , HNullifiable( HConstrainTag, hencodeTag, hdecodeTag, hnullifier, hunnullifier )
  , runTag, unnull
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.TypeLits ( KnownSymbol )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate
  ( Aggregate, Col(..)
  , mapInputs
  , unsafeMakeAggregate
  )
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( nullify, unsafeUnnullify )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.Context ( Interpretation )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name( Name ), Col(..) )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ), Sql )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Schema.Spec.ConstrainDBType
  ( ConstrainDBType
  , dbTypeDict, dbTypeNullity, fromNullityDict
  )
import Rel8.Table.Tag ( Tag(..), Taggable, fromAggregate, fromExpr, fromName )


type Nullifiable :: K.Context -> Constraint
class Interpretation context => Nullifiable context where
  type ConstrainTag context :: Type -> Constraint
  type ConstrainTag _context = DefaultConstrainTag

  encodeTag ::
    ( Sql (ConstrainTag context) a
    , KnownSymbol label
    , Taggable a
    )
    => Tag label a
    -> Col context ('Spec labels 'Required a)

  decodeTag ::
    ( Sql (ConstrainTag context) a
    , KnownSymbol label
    , Taggable a
    )
    => Col context ('Spec labels 'Required a)
    -> Tag label a

  nullifier :: ()
    => Tag label a
    -> (Expr a -> Expr Bool)
    -> SSpec ('Spec labels necessity x)
    -> Col context ('Spec labels necessity x)
    -> Col context ('Spec labels necessity (Nullify x))

  unnullifier :: ()
    => SSpec ('Spec labels necessity x)
    -> Col context ('Spec labels necessity (Nullify x))
    -> Col context ('Spec labels necessity x)


instance Nullifiable Aggregate where
  encodeTag Tag {aggregator, expr} =
    Aggregation $ unsafeMakeAggregate toPrimExpr fromPrimExpr aggregator expr

  decodeTag (Aggregation aggregate) = fromAggregate aggregate

  nullifier Tag {expr} test SSpec {nullity} (Aggregation aggregate) =
    Aggregation $
    mapInputs (toPrimExpr . runTag nullity condition . fromPrimExpr) $
    runTag nullity condition <$> aggregate
    where
      condition = test expr

  unnullifier SSpec {nullity} (Aggregation aggregate) =
    Aggregation $ unnull nullity <$> aggregate

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


instance Nullifiable Expr where
  encodeTag Tag {expr} = DB expr
  decodeTag (DB a) = fromExpr a
  nullifier Tag {expr} test SSpec {nullity} (DB a) =
    DB $ runTag nullity (test expr) a
  unnullifier SSpec {nullity} (DB a) = DB $ unnull nullity a

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


instance Nullifiable Name where
  encodeTag Tag {name = Name name} = NameCol name
  decodeTag (NameCol name) = fromName (Name name)
  nullifier _ _ _ (NameCol name) = NameCol name
  unnullifier _ (NameCol name) = NameCol name

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


runTag :: Nullity a -> Expr Bool -> Expr a -> Expr (Nullify a)
runTag nullity tag a = case nullity of
  Null -> boolExpr null a tag
  NotNull -> boolExpr null (nullify a) tag
  where
    null = fromPrimExpr $ Opaleye.ConstExpr Opaleye.NullLit


unnull :: Nullity a -> Expr (Nullify a) -> Expr a
unnull nullity a = case nullity of
  Null -> a
  NotNull -> unsafeUnnullify a


type HNullifiable :: K.HContext -> Constraint
class HNullifiable context where
  type HConstrainTag context :: Type -> Constraint
  type HConstrainTag _context = DefaultConstrainTag

  hencodeTag :: (Sql (HConstrainTag context) a, KnownSymbol label, Taggable a)
    => Tag label a
    -> context ('Spec labels 'Required a)

  hdecodeTag :: (Sql (HConstrainTag context) a, KnownSymbol label, Taggable a)
    => context ('Spec labels 'Required a)
    -> Tag label a

  hnullifier :: ()
    => Tag label a
    -> (Expr a -> Expr Bool)
    -> SSpec ('Spec labels necessity x)
    -> context ('Spec labels necessity x)
    -> context ('Spec labels necessity (Nullify x))

  hunnullifier :: ()
    => SSpec ('Spec labels necessity x)
    -> context ('Spec labels necessity (Nullify x))
    -> context ('Spec labels necessity x)


instance Nullifiable context => HNullifiable (Col context) where
  type HConstrainTag (Col context) = ConstrainTag context
  hencodeTag = encodeTag
  hdecodeTag = decodeTag
  hnullifier = nullifier
  hunnullifier = unnullifier


instance HNullifiable (Dict (ConstrainDBType constraint)) where
  type HConstrainTag (Dict (ConstrainDBType constraint)) = constraint

  hencodeTag _ = Dict
  hdecodeTag = mempty

  hnullifier _ _ SSpec {} dict = case dbTypeDict dict of
    Dict -> case dbTypeNullity dict of
      Null -> Dict
      NotNull -> Dict

  hunnullifier SSpec {nullity} dict = case dbTypeDict dict of
    Dict -> case nullity of
      Null -> Dict
      NotNull -> case dbTypeNullity dict of
        Null -> fromNullityDict nullity Dict


type DefaultConstrainTag :: Type -> Constraint
class DefaultConstrainTag a
instance DefaultConstrainTag a
