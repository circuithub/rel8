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
  ( Nullifiable( encodeTag, decodeTag, nullifier, unnullifier )
  , runTag, unnull
  )
where

-- base
import Data.Kind ( Constraint )
import GHC.TypeLits ( KnownSymbol )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate
  ( Aggregate( Aggregate ), Col( A )
  , mapInputs
  , unsafeMakeAggregate
  )
import Rel8.Expr ( Expr, Col( E ) )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( nullify, unsafeUnnullify )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import Rel8.Schema.Context ( Interpretation )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name( Name ), Col( N ) )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Table.Tag ( Tag(..), Taggable, fromAggregate, fromExpr, fromName )


type Nullifiable :: K.Context -> Constraint
class Interpretation context => Nullifiable context where
  encodeTag :: Taggable a
    => Tag label a
    -> Col context ('Spec a)

  decodeTag :: (KnownSymbol label, Taggable a)
    => Col context ('Spec a)
    -> Tag label a

  nullifier :: ()
    => Tag label a
    -> (Expr a -> Expr Bool)
    -> SSpec ('Spec x)
    -> Col context ('Spec x)
    -> Col context ('Spec (Nullify x))

  unnullifier :: ()
    => SSpec ('Spec x)
    -> Col context ('Spec (Nullify x))
    -> Col context ('Spec x)


instance Nullifiable Aggregate where
  encodeTag Tag {aggregator, expr} =
    A $ unsafeMakeAggregate toPrimExpr fromPrimExpr aggregator expr

  decodeTag (A aggregate) = fromAggregate aggregate

  nullifier Tag {expr} test SSpec {nullity} (A (Aggregate a)) =
    A $
    mapInputs (toPrimExpr . runTag nullity condition . fromPrimExpr) $
    Aggregate $
    runTag nullity condition <$> a
    where
      condition = test expr

  unnullifier SSpec {nullity} (A (Aggregate a)) =
    A (Aggregate (unnull nullity <$> a))

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


instance Nullifiable Expr where
  encodeTag Tag {expr} = E expr
  decodeTag (E a) = fromExpr a
  nullifier Tag {expr} test SSpec {nullity} (E a) =
    E $ runTag nullity (test expr) a
  unnullifier SSpec {nullity} (E a) = E $ unnull nullity a

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


instance Nullifiable Name where
  encodeTag Tag {name} = N name
  decodeTag (N name) = fromName name
  nullifier _ _ _ (N (Name name)) = N (Name name)
  unnullifier _ (N (Name name)) = N (Name name)

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
