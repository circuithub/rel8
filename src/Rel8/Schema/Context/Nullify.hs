{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Context.Nullify
  ( Nullifiability(..), NonNullifiability(..), nullifiableOrNot, absurd
  , Nullifiable, nullifiability
  , guarder, nullifier, unnullifier
  , sguard, snullify
  )
where

-- base
import Data.Bool ( bool )
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Constraint, Type )
import Data.Monoid ( getFirst )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregate( Aggregate ), foldInputs, mapInputs )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( nullify, unsafeUnnullify )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import Rel8.Kind.Context ( SContext(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name( Name ) )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ) )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec(..) )


type Nullifiability :: K.Context -> Type
data Nullifiability context where
  NAggregate :: Nullifiability Aggregate
  NExpr :: Nullifiability Expr
  NName :: Nullifiability Name


type Nullifiable :: K.Context -> Constraint
class Nullifiable context where
  nullifiability :: Nullifiability context


instance Nullifiable Aggregate where
  nullifiability = NAggregate


instance Nullifiable Expr where
  nullifiability = NExpr


instance Nullifiable Name where
  nullifiability = NName


type NonNullifiability :: K.Context -> Type
data NonNullifiability context where
  NNResult :: NonNullifiability Result


nullifiableOrNot :: ()
  => SContext context
  -> Either (NonNullifiability context) (Nullifiability context)
nullifiableOrNot = \case
  SAggregate -> Right NAggregate
  SExpr -> Right NExpr
  SName -> Right NName
  SResult -> Left NNResult


absurd :: Nullifiability context -> NonNullifiability context -> a
absurd = \case
  NAggregate -> \case
  NExpr -> \case
  NName -> \case


guarder :: ()
  => SContext context
  -> context tag
  -> (tag -> Bool)
  -> (Expr tag -> Expr Bool)
  -> context (Maybe a)
  -> context (Maybe a)
guarder SAggregate tag _ isNonNull (Aggregate a) =
  mapInputs (toPrimExpr . run . fromPrimExpr) $
  Aggregate $
  run <$> a
  where
    mtag = foldInputs (\_ -> pure . fromPrimExpr) tag
    run = maybe id (sguard . isNonNull) (getFirst mtag)
guarder SExpr tag _ isNonNull a = sguard condition a
  where
    condition = isNonNull tag
guarder SName _ _ _ name = name
guarder SResult (Identity tag) isNonNull _ (Identity a) =
  Identity (bool Nothing a condition)
  where
    condition = isNonNull tag


nullifier :: ()
  => Nullifiability context
  -> Spec a
  -> context a
  -> context (Nullify a)
nullifier NAggregate Spec {nullity} (Aggregate a) =
  Aggregate $ snullify nullity <$> a
nullifier NExpr Spec {nullity} a = snullify nullity a
nullifier NName _ (Name a) = Name a


unnullifier :: ()
  => Nullifiability context
  -> Spec a
  -> context (Nullify a)
  -> context a
unnullifier NAggregate Spec {nullity} (Aggregate a) =
  Aggregate $ sunnullify nullity <$> a
unnullifier NExpr Spec {nullity} a = sunnullify nullity a
unnullifier NName _ (Name a) = Name a


sguard :: Expr Bool -> Expr (Maybe a) -> Expr (Maybe a)
sguard condition a = boolExpr null a condition
  where
    null = fromPrimExpr $ Opaleye.ConstExpr Opaleye.NullLit


snullify :: Nullity a -> Expr a -> Expr (Nullify a)
snullify nullity a = case nullity of
  Null -> a
  NotNull -> nullify a


sunnullify :: Nullity a -> Expr (Nullify a) -> Expr a
sunnullify nullity a = case nullity of
  Null -> a
  NotNull -> unsafeUnnullify a
