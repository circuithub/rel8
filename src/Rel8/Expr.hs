{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Expr
  ( Expr(..)
  , Col( DB, unDB )
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Type )
import Data.String ( IsString, fromString )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr.Null ( liftOpNullable, nullify )
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr
  , mapPrimExpr
  , zipPrimExprsWith
  )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Context ( Interpretation, Col )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, encodeTag, decodeTag, nullifier, unnullifier
  , runTag, unnull
  )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import Rel8.Schema.Nullability
  ( Nullability( Nullable, NonNullable )
  , Sql, nullabilization
  )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Num ( DBFractional, DBNum )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )


-- | Typed SQL expressions.
type role Expr representational
type Expr :: Type -> Type
newtype Expr a = Expr Opaleye.PrimExpr
  deriving stock Show


instance Sql DBSemigroup a => Semigroup (Expr a) where
  (<>) = case nullabilization @a of
    Nullable -> liftOpNullable (<>.)
    NonNullable -> (<>.)
  {-# INLINABLE (<>) #-}


instance Sql DBMonoid a => Monoid (Expr a) where
  mempty = case nullabilization @a of
    Nullable -> nullify memptyExpr
    NonNullable -> memptyExpr
  {-# INLINABLE mempty #-}


instance (Sql IsString a, Sql DBType a) => IsString (Expr a) where
  fromString = litExpr . case nullabilization @a of
    Nullable -> Just . fromString
    NonNullable -> fromString


instance Sql DBNum a => Num (Expr a) where
  (+) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:+))
  (*) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:*))
  (-) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:-))

  abs = mapPrimExpr (Opaleye.UnExpr Opaleye.OpAbs)
  negate = mapPrimExpr (Opaleye.UnExpr Opaleye.OpNegate)

  signum = castExpr . mapPrimExpr (Opaleye.UnExpr (Opaleye.UnOpOther "SIGN"))

  fromInteger = castExpr . fromPrimExpr . Opaleye.ConstExpr . Opaleye.IntegerLit


instance Sql DBFractional a => Fractional (Expr a) where
  (/) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:/))

  fromRational =
    castExpr . Expr . Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac


instance Interpretation Expr where
  data Col Expr _spec where
    DB :: {unDB :: Expr a} -> Col Expr ('Spec labels necessity a)


instance Sql DBType a => Table Expr (Expr a) where
  type Columns (Expr a) = HType a
  type Context (Expr a) = Expr

  toColumns a = HType (DB a)
  fromColumns (HType (DB a)) = a


instance Sql DBType a => Recontextualize Expr Expr (Expr a) (Expr a)


instance Sql DBType a => Recontextualize Expr Identity (Expr a) (Identity a)


instance Sql DBType a => Recontextualize Identity Expr (Identity a) (Expr a)


instance Labelable Expr where
  labeler (DB a) = DB a
  unlabeler (DB a) = DB a


instance Nullifiable Expr where
  encodeTag = DB
  decodeTag (DB a) = a
  nullifier tag SSpec {nullability} (DB a) = DB $ runTag nullability tag a
  unnullifier _ SSpec {nullability} (DB a) = DB $ unnull nullability a

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}
