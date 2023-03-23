{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Serialize
  ( Serializable, lit, litHTable, parse
  , ToExprs
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Serialize ( slitExpr, sparseValue )
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.Null ( NotNull, Sql )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec(..) )
import Rel8.Table ( Table, fromColumns, FromExprs, fromResult, toResult )
import Rel8.Type ( DBType )

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )


-- | @ToExprs exprs a@ is evidence that the types @exprs@ and @a@ describe
-- essentially the same type, but @exprs@ is in the 'Expr' context, and @a@ is
-- a normal Haskell type.
type ToExprs :: Type -> Type -> Constraint
class Table Expr exprs => ToExprs exprs a


instance {-# OVERLAPPABLE #-} (Sql DBType a, x ~ Expr a) => ToExprs x a


instance (Sql DBType a, x ~ [a]) => ToExprs (Expr x) [a]


instance (Sql DBType a, NotNull a, x ~ Maybe a) => ToExprs (Expr x) (Maybe a)


instance (Sql DBType a, NotNull a, x ~ NonEmpty a) => ToExprs (Expr x) (NonEmpty a)


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ (exprs1, exprs2)) =>
  ToExprs x (a, b)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , x ~ (exprs1, exprs2, exprs3)
  )
  => ToExprs x (a, b, c)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , x ~ (exprs1, exprs2, exprs3, exprs4)
  )
  => ToExprs x (a, b, c, d)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5)
  )
  => ToExprs x (a, b, c, d, e)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , ToExprs exprs6 f
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5, exprs6)
  )
  => ToExprs x (a, b, c, d, e, f)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , ToExprs exprs6 f
  , ToExprs exprs7 g
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5, exprs6, exprs7)
  )
  => ToExprs x (a, b, c, d, e, f, g)


-- | @Serializable@ witnesses the one-to-one correspondence between the type
-- @sql@, which contains SQL expressions, and the type @haskell@, which
-- contains the Haskell decoding of rows containing @sql@ SQL expressions.
type Serializable :: Type -> Type -> Constraint
class (ToExprs exprs a, a ~ FromExprs exprs) => Serializable exprs a | exprs -> a
instance (ToExprs exprs a, a ~ FromExprs exprs) => Serializable exprs a
instance {-# OVERLAPPING #-} Sql DBType a => Serializable (Expr a) a


-- | Use @lit@ to turn literal Haskell values into expressions. @lit@ is
-- capable of lifting single @Expr@s to full tables.
lit :: forall exprs a. Serializable exprs a => a -> exprs
lit = fromColumns . litHTable . toResult @_ @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromResult @_ @exprs <$> parseHTable


litHTable :: HTable t => t Result -> t Expr
litHTable as = htabulate $ \field ->
  case hfield hspecs field of
    Spec {nullity, info} -> case hfield as field of
      Identity value -> slitExpr nullity info value


parseHTable :: HTable t => Hasql.Row (t Result)
parseHTable = unwrapApplicative $ htabulateA $ \field ->
  WrapApplicative $ case hfield hspecs field of
    Spec {nullity, info} -> Identity <$> sparseValue nullity info
