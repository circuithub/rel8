{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Context.Nullify
  ( Nullifiable, nullifier, unnullifier
  , guardExpr, snullify
  )
where

-- base
import Data.Kind ( Constraint )
import Data.Monoid ( getFirst )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate
  ( Aggregate( Aggregate ), Col( A )
  , foldInputs, mapInputs
  )
import Rel8.Expr ( Expr, Col( E ) )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( nullify, unsafeUnnullify )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name( Name ), Col( N ) )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ), Sql )
import Rel8.Schema.Reify ( Reify, Col( Reify ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Type ( DBType )


type Nullifiable :: K.Context -> Constraint
class Nullifiable context where
  nullifier :: Sql DBType tag
    => Col context ('Spec tag)
    -> (Expr tag -> Expr Bool)
    -> SSpec ('Spec a)
    -> Col context ('Spec a)
    -> Col context ('Spec (Nullify a))

  unnullifier :: ()
    => SSpec ('Spec a)
    -> Col context ('Spec (Nullify a))
    -> Col context ('Spec a)


instance Nullifiable Aggregate where
  nullifier (A tag) isNonNull SSpec {nullity} (A (Aggregate a)) =
    A $
    mapInputs (toPrimExpr . run . fromPrimExpr) $
    Aggregate $
    run <$> a
    where
      mtag = foldInputs (\_ -> pure . fromPrimExpr) tag
      run = maybe id (guardExpr . isNonNull) (getFirst mtag) . snullify nullity

  unnullifier SSpec {nullity} (A (Aggregate a)) =
    A $ Aggregate $ sunnullify nullity <$> a


instance Nullifiable Expr where
  nullifier (E tag) isNonNull SSpec {nullity} (E a) =
    E $ guardExpr condition (snullify nullity a)
    where
      condition = isNonNull tag

  unnullifier SSpec {nullity} (E a) = E $ sunnullify nullity a


instance Nullifiable Name where
  nullifier _ _ _ (N (Name a)) = N $ Name a
  unnullifier _ (N (Name a)) = N $ Name a


instance Nullifiable context => Nullifiable (Reify context) where
  nullifier (Reify tag) isNonNull spec (Reify a) =
    Reify $ nullifier tag isNonNull spec a

  unnullifier spec (Reify a) = Reify (unnullifier spec a)


guardExpr :: Expr Bool -> Expr (Maybe a) -> Expr (Maybe a)
guardExpr condition a = boolExpr null a condition
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
