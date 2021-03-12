{-# language DataKinds #-}

module Rel8.Expr.Null
  ( null, nullable, nullableOf
  , isNull, isNonNull
  , nullify, seminullify
  , mapNullable, liftOpNullable
  , unsafeUnnullify, unsafeSemiunnullify
  , unsafeMapNullable, unsafeMapSeminullable
  , unsafeLiftOpNullable, unsafeLiftOpSeminullable
  )
where

-- base
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr
  ( Expr( Expr )
  , null
  , seminullify
  , unsafeUnnullify
  , unsafeLiftOpSeminullable
  )
import Rel8.Expr.Bool ( boolExpr, (||.) )
import Rel8.Kind.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Type ( DBType )


nullable :: ()
  => Expr nullability b
  -> (Expr 'NonNullable a -> Expr nullability b)
  -> Expr 'Nullable a
  -> Expr nullability b
nullable b f ma = boolExpr (f (unsafeUnnullify ma)) b (isNull ma)


nullableOf :: DBType a => Maybe (Expr 'NonNullable a) -> Expr 'Nullable a
nullableOf = maybe null nullify


isNull :: Expr 'Nullable a -> Expr 'NonNullable Bool
isNull (Expr a) = Expr (Opaleye.UnExpr Opaleye.OpIsNull a)


isNonNull :: Expr 'Nullable a -> Expr 'NonNullable Bool
isNonNull (Expr a) = Expr (Opaleye.UnExpr Opaleye.OpIsNotNull a)


mapNullable :: DBType b
  => (Expr 'NonNullable a -> Expr 'NonNullable b)
  -> Expr 'Nullable a -> Expr 'Nullable b
mapNullable f ma = boolExpr (unsafeMapNullable f ma) null (isNull ma)


liftOpNullable :: DBType c
  => (Expr 'NonNullable a -> Expr 'NonNullable b -> Expr 'NonNullable c)
  -> Expr 'Nullable a -> Expr 'Nullable b -> Expr 'Nullable c
liftOpNullable f ma mb =
  boolExpr (unsafeLiftOpNullable f ma mb) null
    (isNull ma ||. isNull mb)


nullify :: Expr nullability a -> Expr 'Nullable a
nullify (Expr a) = Expr a


unsafeSemiunnullify :: Expr 'Nullable a -> Expr nullability a
unsafeSemiunnullify (Expr a) = Expr a


unsafeMapNullable :: ()
  => (Expr nullability a -> Expr nullability b)
  -> Expr 'Nullable a -> Expr 'Nullable b
unsafeMapNullable f ma = nullify (f (unsafeSemiunnullify ma))


unsafeMapSeminullable :: ()
  => (Expr 'NonNullable a -> Expr 'NonNullable b)
  -> Expr nullability a -> Expr nullability b
unsafeMapSeminullable f ma = seminullify (f (unsafeUnnullify ma))


unsafeLiftOpNullable :: ()
  => (Expr nullability a -> Expr nullability b -> Expr nullability c)
  -> Expr 'Nullable a -> Expr 'Nullable b -> Expr 'Nullable c
unsafeLiftOpNullable f ma mb =
  nullify (f (unsafeSemiunnullify ma) (unsafeSemiunnullify mb))
