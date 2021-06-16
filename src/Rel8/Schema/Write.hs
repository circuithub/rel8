{-# language DataKinds #-}
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

module Rel8.Schema.Write
  ( Writable( Default, Value )
  , Write, fromWrite, unwrite
  , Writes
  , Col( W, unW )
  ) where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Data.String ( IsString, fromString )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Defaulting
  ( Defaulting( HasDefault, NoDefault )
  , KnownDefaulting
  )
import Rel8.Schema.Context ( Interpretation( Col ) )
import Rel8.Schema.Context.Label ( Labelable( labeler, unlabeler) )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, encodeTag, decodeTag, nullifier, unnullifier
  , runTag, unnull
  )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( notReify )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec( SSpec, nullity ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Tag ( Tag( Tag, expr ), fromExpr )
import Rel8.Type ( DBType )
import Rel8.Type.Monoid ( DBMonoid )
import Rel8.Type.Num ( DBFloating, DBFractional, DBNum )
import Rel8.Type.Semigroup ( DBSemigroup )


type Writable :: Defaulting -> k -> Type
data Writable defaulting a where
  Default :: k ~ Type => Writable 'HasDefault (a :: k)
  Value :: k ~ Type => Expr a -> Writable defaulting (a :: k)


fromWrite :: Writable defaulting a -> Maybe (Expr a)
fromWrite Default = Nothing
fromWrite (Value a) = Just a


unwrite :: Write a -> Expr a
unwrite (Value a) = a


instance Sql DBSemigroup a => Semigroup (Writable defaulting a) where
  (<>) = zipWrites (<>)


instance Sql DBMonoid a => Monoid (Writable defaulting a) where
  mempty = Value mempty


instance (Sql IsString a, Sql DBType a) => IsString (Writable defaulting a) where
  fromString = Value . fromString


instance Sql DBNum a => Num (Writable defaulting a) where
  (+) = zipWrites (+)
  (*) = zipWrites (*)
  (-) = zipWrites (-)
  abs = mapWrite abs
  negate = mapWrite negate
  signum = mapWrite signum
  fromInteger = Value . fromInteger


instance Sql DBFractional a => Fractional (Writable defaulting a) where
  (/) = zipWrites (/)
  fromRational = Value . fromRational


instance Sql DBFloating a => Floating (Writable defaulting a) where
  pi = Value pi
  exp = mapWrite exp
  log = mapWrite log
  sqrt = mapWrite sqrt
  (**) = zipWrites (**)
  logBase = zipWrites logBase
  sin = mapWrite sin
  cos = mapWrite cos
  tan = mapWrite tan
  asin = mapWrite asin
  acos = mapWrite acos
  atan = mapWrite atan
  sinh = mapWrite sinh
  cosh = mapWrite cosh
  tanh = mapWrite tanh
  asinh = mapWrite atanh
  acosh = mapWrite acosh
  atanh = mapWrite atanh


mapWrite :: ()
  => (Expr (a :: Type) -> Expr (b :: Type))
  -> Writable defaulting a -> Writable defaulting b
mapWrite _ Default = Default
mapWrite f (Value a) = Value (f a)


zipWrites :: ()
  => (Expr (a :: Type) -> Expr (b :: Type) -> Expr (c :: Type))
  -> Writable defaulting a -> Writable defaulting b -> Writable defaulting c
zipWrites _ Default _ = Default
zipWrites _ _ Default = Default
zipWrites f (Value a) (Value b) = Value (f a b)


type Write :: k -> Type
type Write = Writable 'NoDefault


instance (KnownDefaulting defaulting, Sql DBType a) =>
  Table Write (Writable defaulting a)
 where
  type Columns (Writable defaulting a) = HIdentity ('Spec '[] defaulting a)
  type Context (Writable defaulting a) = Write

  toColumns = HIdentity . W
  fromColumns (HIdentity (W a)) = a
  reify = notReify
  unreify = notReify


instance Interpretation Write where
  data Col Write _spec where
    W :: {unW :: !(Writable defaulting a)} -> Col Write ('Spec labels defaulting a)


instance Sql DBType a => Recontextualize Aggregate Write (Aggregate a) (Write a)


instance Sql DBType a => Recontextualize Expr Write (Expr a) (Write a)


instance Sql DBType a => Recontextualize Result Write (Identity a) (Write a)


instance Sql DBType a => Recontextualize Write Aggregate (Write a) (Aggregate a)


instance Sql DBType a => Recontextualize Write Expr (Write a) (Expr a)


instance Sql DBType a => Recontextualize Write Result (Write a) (Identity a)


instance Sql DBType a => Recontextualize Write Write (Write a) (Write a)


instance Sql DBType a => Recontextualize Write Name (Write a) (Name a)


instance Sql DBType a => Recontextualize Name Write (Name a) (Write a)


instance Labelable Write where
  labeler (W a) = W a
  unlabeler (W a) = W a


instance Nullifiable Write where
  encodeTag = W . Value . expr

  decodeTag (W (Value a)) = fromExpr a

  nullifier Tag {expr} test SSpec {nullity} = \case
    W Default -> W Default
    W (Value a) -> W $ Value $ runTag nullity condition a
    where
      condition = test expr

  unnullifier SSpec {nullity} = \case
    W Default -> W Default
    W (Value a) -> W $ Value $ unnull nullity a

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


-- | @Writes a b@ means that the columns in @a@ are compatible for inserting
-- with the table @b@.
type Writes :: Type -> Type -> Constraint
class Recontextualize Expr Write exprs writes => Writes exprs writes
instance Recontextualize Expr Write exprs writes => Writes exprs writes
