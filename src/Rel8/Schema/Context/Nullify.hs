{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context.Nullify
  ( Nullifiable( ConstrainTag, encodeTag, decodeTag, nullifier, unnullifier )
  , HNullifiable( HConstrainTag, hencodeTag, hdecodeTag, hnullifier, hunnullifier )
  , unnull, runTag
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( nullify, unsafeUnnullify )
import Rel8.Expr.Opaleye ( fromPrimExpr )
import Rel8.Kind.Labels ( KnownLabels )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.Context ( Interpretation, Col(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Nullability
  ( Nullify
  , Nullability( Nullable, NonNullable ), nullabilization
  , Sql
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Schema.Spec.ConstrainDBType
  ( ConstrainDBType
  , dbTypeDict, dbTypeNullability, fromNullabilityDict
  )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )


type Nullifiable :: K.Context -> Constraint
class Interpretation context => Nullifiable context where
  type ConstrainTag context :: Type -> Constraint
  type ConstrainTag _context = DefaultConstrainTag

  encodeTag :: (Sql (ConstrainTag context) a, Sql DBEq a, KnownLabels labels)
    => Expr a
    -> Col context ('Spec labels 'Required a)

  decodeTag :: (Sql (ConstrainTag context) a, Sql DBMonoid a)
    => Col context ('Spec labels 'Required a)
    -> Expr a

  nullifier :: ()
    => Expr Bool
    -> SSpec ('Spec labels necessity a)
    -> Col context ('Spec labels necessity a)
    -> Col context ('Spec labels necessity (Nullify a))

  unnullifier :: ()
    => Expr Bool
    -> SSpec ('Spec labels necessity a)
    -> Col context ('Spec labels necessity (Nullify a))
    -> Col context ('Spec labels necessity a)


runTag :: Nullability a -> Expr Bool -> Expr a -> Expr (Nullify a)
runTag nullability tag a = case nullability of
  Nullable -> boolExpr null a tag
  NonNullable -> boolExpr null (nullify a) tag
  where
    null = fromPrimExpr $ Opaleye.ConstExpr Opaleye.NullLit


unnull :: Nullability a -> Expr (Nullify a) -> Expr a
unnull nullability a = case nullability of
  Nullable -> a
  NonNullable -> unsafeUnnullify a


type HNullifiable :: K.HContext -> Constraint
class HNullifiable context where
  type HConstrainTag context :: Type -> Constraint
  type HConstrainTag _context = DefaultConstrainTag

  hencodeTag :: (Sql (HConstrainTag context) a, Sql DBEq a, KnownLabels labels)
    => Expr a
    -> context ('Spec labels 'Required a)

  hdecodeTag :: (Sql (HConstrainTag context) a, Sql DBMonoid a)
    => context ('Spec labels 'Required a)
    -> Expr a

  hnullifier :: ()
    => Expr Bool
    -> SSpec ('Spec labels necessity a)
    -> context ('Spec labels necessity a)
    -> context ('Spec labels necessity (Nullify a))

  hunnullifier :: ()
    => Expr Bool
    -> SSpec ('Spec labels necessity a)
    -> context ('Spec labels necessity (Nullify a))
    -> context ('Spec labels necessity a)


instance Nullifiable context => HNullifiable (Col context) where
  type HConstrainTag (Col context) = ConstrainTag context
  hencodeTag = encodeTag
  hdecodeTag = decodeTag
  hnullifier = nullifier
  hunnullifier = unnullifier


instance HNullifiable (Dict (ConstrainDBType constraint)) where
  type HConstrainTag (Dict (ConstrainDBType constraint)) = constraint

  hencodeTag _ = Dict

  hdecodeTag :: forall a context labels. (Sql (HConstrainTag context) a, Sql DBMonoid a)
    => context ('Spec labels 'Required a)
    -> Expr a
  hdecodeTag _ = case nullabilization @a of
    Nullable -> nullify memptyExpr
    NonNullable -> memptyExpr


  hnullifier _ SSpec {} dict = case dbTypeDict dict of
    Dict -> case dbTypeNullability dict of
      Nullable -> Dict
      NonNullable -> Dict

  hunnullifier _ SSpec {nullability} dict = case dbTypeDict dict of
    Dict -> case nullability of
      Nullable -> Dict
      NonNullable -> case dbTypeNullability dict of
        Nullable -> fromNullabilityDict nullability Dict


type DefaultConstrainTag :: Type -> Constraint
class DefaultConstrainTag a
instance DefaultConstrainTag a
